package HierarchyChecker

import AST._

import scala.language.postfixOps

/**
  * A pass to check the following:
  *   A nonstatic method must not replace a static method
  *   A class that contains (declares or inherits) any abstract methods must be abstract
  *   A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
  *   A method must not replace a method with a different return type
  *   A protected method must not replace a public method
  *   A method must not replace a final method
  *
  * this pass analyses:
  *
  */
class MethodsPass(checker: HierarchyChecker, ast: CompilationUnit) extends Visitor  {

  private val myMethods: Map[MethodSig, MethodDecl]
    = checker.declaredMethods(ast.typeDecl)
  private val myConstructors: Map[ConstructorSig, ConstructorDecl]
    = checker.declaredConstructors(ast.typeDecl)

  def getMethodsFromID(id: FullyQualifiedID): Map[MethodSig, MethodDecl]
    = checker.declaredMethods( checker.resolve(id, ast) )

  def commonRules(concreteInheritance: Map[MethodSig, MethodDecl],
                  abstractInheritance: List[Map[MethodSig, MethodDecl]]): Unit = {
    val allMethodSigs: Iterable[MethodSig] = myMethods.keys ++ concreteInheritance.keys ++
      abstractInheritance.foldLeft(Iterable[MethodSig]())(_ ++ _.keys)
    val loc = Some(Error.Location(ast.typeDecl.name.row, ast.typeDecl.name.col, ast.fileName))

    for(sig <- allMethodSigs) {
      { // A nonstatic method must not replace a static method
        val areStatic = (myMethods :: concreteInheritance :: abstractInheritance).collect {
          case map if map.contains(sig) => map(sig).modifiers.exists(_.isInstanceOf[Token.JavaStatic])
        }
        if(areStatic.exists(_ != areStatic.head))
          throw Error.Error(sig.toString,
            "A nonstatic method must not replace a static method",
            Error.Type.MethodsPass, loc)
      }
      {
        // A class must not contain (declare or inherit) two methods with the same signature but different return types
        // A method must not replace a method with a different return type
        val returnTypes = (myMethods :: concreteInheritance :: abstractInheritance).collect {
          case map if map.contains(sig) => map(sig).returnType
        }
        val returnTypeSigs = returnTypes.collect {
          case Some(t) => checker.makeSig(t, ast)
          case None => voidSig()
        }
        if(returnTypeSigs.exists(_ != returnTypeSigs.head))
          throw Error.Error(sig + " => " + returnTypes.mkString(", "),
            "A class must not declare or inherit two methods with the same signature but different return types",
            Error.Type.MethodsPass, loc)
      }
      { // A protected method must not replace a public method
        if(myMethods.contains(sig) && myMethods(sig).modifiers.exists(_.isInstanceOf[Token.JavaProtected]))
          (concreteInheritance :: abstractInheritance).foreach { map =>
            if (map.contains(sig) && map(sig).modifiers.exists(_.isInstanceOf[Token.JavaPublic]))
              throw Error.Error(sig + " => " + map(sig).name.lexeme,
                "A protected method must not replace a public method",
                Error.Type.MethodsPass, loc)
          }
      }
      { // A method must not replace a final method
        if(myMethods.contains(sig))
          (concreteInheritance :: abstractInheritance).foreach { map =>
            if(map.contains(sig) && map(sig).modifiers.exists(_.isInstanceOf[Token.JavaFinal]))
              throw Error.Error(sig + " => " + map(sig).name.lexeme,
                "A method must not replace a final method",
                Error.Type.MethodsPass, loc)
          }
      }
    }
  }

  override def visit(cd: ClassDecl): Unit = {
    //TODO: fix so that the hieratence follows up the tree
    val abstractInheritance = for(inter <- cd.implementationOf) yield getMethodsFromID(inter)
    val objectClassInheritance = if(cd.id != checker.objectClass._1) checker.objectClass._2
                                  else Map[MethodSig, MethodDecl]()
    val concreteInheritance = objectClassInheritance ++ (cd.extensionOf match {
      case Some(extend) => getMethodsFromID(extend)
      case None => Map()
    })

    commonRules(concreteInheritance,abstractInheritance)

    { //A class that contains (declares or inherits) any abstract methods must be abstract
      if(!cd.modifiers.exists(_.isInstanceOf[Token.JavaAbstract]))
        for(inheritMap <- abstractInheritance; sig <- inheritMap.keys) {
          if(!myMethods.contains(sig) && !concreteInheritance.contains(sig))
            throw Error.Error("class: " + cd.name.lexeme + " does not implement " + sig +
              " as required by interface " + cd.implementationOf(abstractInheritance.indexOf(inheritMap)).id.lexeme,
              "A class that contains (declares or inherits) any abstract methods must be abstract",
              Error.Type.MethodsPass, Some(Error.Location(cd.name.row, cd.name.col, ast.fileName)))
        }
    }

  }

  override def visit(id: InterfaceDecl): Unit = {
    val abstractInheritance = for(inter <- id.extensionOf) yield getMethodsFromID(inter)
    val concreteInheritance = checker.objectClass._2
    commonRules(concreteInheritance, abstractInheritance)
  }
}
