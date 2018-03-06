package HierarchyChecker

import AST._

import scala.collection.mutable
import scala.concurrent.Future
import scala.language.postfixOps

class HierarchyChecker(val localContexts: Map[CompilationUnit, Map[String, TypeDecl]],
                       val typeContext: Map[String, List[TypeDecl]]) {
  val localContextsByTypeDecl: Map[TypeDecl, Map[String, TypeDecl]] = localContexts.map(context => context._1.typeDecl -> context._2)

  /**
    *  The hierarchy must be acyclic
    */
  def checkForCycles(): Future[Unit] = {
    val checkedUnits: mutable.Set[TypeDecl] = mutable.Set() // memoization bc/ dynamic programming is hard
    def checkIfCyclic(unit: TypeDecl, visited: List[TypeDecl]): Unit = {
      if (visited.contains(unit))
        throw Error.Error(visited.map(decl => decl.name.lexeme).mkString(" -> "),
          "The hierarchy is cyclic", Error.Type.HierarchyCheck)

      if (! checkedUnits.contains(unit)) {
        unit match {
          case i: InterfaceDecl =>
            for(extend <- i.extensionOf)
              checkIfCyclic(resolve(extend, unit), unit :: visited)

          case c: ClassDecl =>
            c.extensionOf match {
              case Some(extend) =>
                println(unit.name.lexeme + ":" + extend.name)
                checkIfCyclic(resolve(extend, unit), unit :: visited)
              case None =>
            }
            for(imp <- c.implementationOf)
              checkIfCyclic(resolve(imp, unit), unit :: visited)
        }

        checkedUnits += unit
      }
    }
    for (unit <- localContexts.keys.toList){
      checkIfCyclic(unit.typeDecl, List())
    }
    Future.successful(())
  }

  /**
    * @return what this full qualified id, used in this ast, refers to in the hierarchy
    */
  def resolve(fqid: FullyQualifiedID, ast: CompilationUnit): TypeDecl
    = resolve(fqid, ast.typeDecl)
  def resolve(fqid: FullyQualifiedID, td: TypeDecl): TypeDecl = {
    def findInPack(name: String, types: List[TypeDecl]): TypeDecl = {
      for(t <- types)
        if(t.name.lexeme == name)
          return t
      assert(assertion = false, s"Type $name does not exist in hierarchy"); types.head
    }
    if ( fqid.qualifiers.nonEmpty )
      findInPack(fqid.id.lexeme, typeContext(fqid.pack))
    else
      localContextsByTypeDecl(td)(fqid.id.lexeme)
  }

  def makeSig(typ: Type, ast: CompilationUnit): Signature = typ match {
    case ArrayType(arrayOf, _)    => ArraySig(makeSig(arrayOf, ast))
    case PrimitiveType(typeToken) => PrimitiveSig(typeToken.lexeme)
    case ClassType(typeID)        => ClassSig(resolve(typeID, ast).hashCode())
  }

  /**
    * Maps each type to a map of each method signature and declaration
    */
  lazy val declaredMethods: Map[TypeDecl, Map[MethodSig, MethodDecl]] = {
    def getMethods(ast: CompilationUnit): Map[MethodSig, MethodDecl] =  {
      val members = ast.typeDecl match {
        case id: InterfaceDecl => id.members
        case cd: ClassDecl => cd.members
      }
      val methods = members.collect { case md: MethodDecl => md }
      val methodSigs: Seq[(MethodSig, MethodDecl)] = methods.map { md =>
        MethodSig(md.name.lexeme, for(pd <- md.parameters) yield makeSig(pd.typ, ast)) -> md
      }

      { // A class must not declare two methods with the same signature
        val methodSigDuplicates = methodSigs.map(m => m._1).groupBy(identity).collect { case (x, List(_, _, _*)) => x }
        if(methodSigDuplicates.nonEmpty)
          throw Error.Error(methodSigDuplicates.mkString("\n"),
            "A class or interface must not declare two methods with the same signature",
            Error.Type.HierarchyCheck, Some(Error.Location(ast.typeDecl.name.row, ast.typeDecl.name.col, ast.fileName)))
      }

      methodSigs.toMap
    }

    localContexts.keys.map { cu: CompilationUnit => cu.typeDecl -> getMethods(cu) } toMap
  }

  /**
    * Maps each type to a map of each constructor signature and declaration
    */
  lazy val declaredConstructors: Map[TypeDecl, Map[ConstructorSig, ConstructorDecl]] = {
    def getCtors(ast: CompilationUnit): Map[ConstructorSig, ConstructorDecl] = {
      val members = ast.typeDecl match {
        case id: InterfaceDecl => id.members
        case cd: ClassDecl => cd.members
      }
      val ctors = members.collect { case cd: ConstructorDecl => cd }
      val ctorSigs: Seq[(ConstructorSig, ConstructorDecl)]  = ctors.map { cd =>
        ConstructorSig(for(pd <- cd.parameters) yield makeSig(pd.typ, ast)) -> cd
      }

      { // A class must not declare two constructors with the same parameter types
        val ctorSigDuplicates = ctorSigs.map(c => c._1).groupBy(identity).collect { case (x, List(_, _, _*)) => x }
        if(ctorSigDuplicates.nonEmpty)
          throw Error.Error(ctorSigDuplicates.mkString("\n"),
            "A class must not declare two constructors with the same parameter types",
            Error.Type.HierarchyCheck, Some(Error.Location(ast.typeDecl.name.row, ast.typeDecl.name.col, ast.fileName)))
      }

      ctorSigs.toMap
    }

    localContexts.keys.map { cu: CompilationUnit => cu.typeDecl -> getCtors(cu) } toMap
  }

  /**
    * run hierarchy checking passes on ast
    */
  def check(ast: CompilationUnit): List[Future[Unit]] = {
    val passes: List[Visitor] = List(
      new ExtendsPass(localContexts(ast), typeContext, ast),
      new MethodsPass(this, ast)
    )
    for(pass <- passes) yield pass.run(ast)
  }
}
