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

  private lazy val myMethods: Map[MethodSig, MethodDecl]
    = checker.declaredMethods(ast.typeDecl)
  private val myConstructors: Map[ConstructorSig, ConstructorDecl]
    = checker.declaredConstructors(ast.typeDecl)

  def getMethodsFromID(id: FullyQualifiedID): Map[MethodSig, MethodDecl]
    = checker.declaredMethods( checker.resolve(id, ast) )

  override def visit(cd: ClassDecl): Unit = {
    val abstractInheritence: List[Map[MethodSig, MethodDecl]]
      = for(inter <- cd.implementationOf) yield getMethodsFromID(inter)
    val concreteInheritence: Map[MethodSig, MethodDecl] = cd.extensionOf match {
      case Some(extend) => getMethodsFromID(extend)
      case None => Map()
    }
    val allMethodSigs = myMethods.keys ++ concreteInheritence.keys ++
      abstractInheritence.foldLeft(Iterable[MethodSig]())(_ ++ _.keys)

    for(sig <- allMethodSigs) {
      { // A method must not replace a method with a different return type
        val returnTypes = (myMethods :: concreteInheritence :: abstractInheritence).collect {
          case map if map.contains(sig) => map(sig).returnType
        }
        if(returnTypes.exists(_ != returnTypes.head))
          throw Error.Error(sig + " => " + returnTypes.mkString(", "),
            "A class must not declare or inherit two methods with the same signature but different return types",
            Error.Type.MethodsPass, Some(Error.Location(cd.name.row, cd.name.col, ast.fileName)))
      }
      {

      }
    }


  }

  override def visit(id: InterfaceDecl): Unit = {
  }
}
