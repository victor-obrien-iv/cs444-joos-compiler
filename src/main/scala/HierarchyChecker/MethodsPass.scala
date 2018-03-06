package HierarchyChecker

import AST._

/**
  * A pass to check the following:
  *   A nonstatic method must not replace a static method
  *   A class that contains (declares or inherits) any abstract methods must be abstract
  *   A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
  *   A class must not declare two constructors with the same parameter types
  *   A method must not replace a method with a different return type
  *   A protected method must not replace a public method
  *   A method must not replace a final method
  *   A class must not declare two methods with the same signature
  *   An interface must not declare two methods with the same signature
  *
  * this pass analyses:
  *
  */
class MethodsPass(checker: HierarchyChecker, ast: CompilationUnit) extends Visitor  {

  private lazy val myMethods: Map[checker.MethodSig, MethodDecl]
    = checker.declaredMethods(ast.typeDecl)
  private lazy val myConstructors: Map[checker.ConstructorSig, ConstructorDecl]
    = checker.declaredConstructors(ast.typeDecl)



  override def visit(cd: ClassDecl): Unit = {

    { // A class must not declare two methods with the same signature
      val methodSigDuplicates = myMethods.keys.groupBy(identity).collect { case (x, List(_, _, _*)) => x }
      if(methodSigDuplicates.nonEmpty)
        throw Error.Error(methodSigDuplicates.mkString("\n"),
          "A class or interface must not declare two methods with the same signature",
          Error.Type.MethodsPass, Some(Error.Location(cd.name.row, cd.name.col, ast.fileName)))
    }

    { // A class must not declare two constructors with the same parameter types
      val ctorSigDuplicates = myConstructors.keys.groupBy(identity).collect { case (x, List(_, _, _*)) => x }
      if(ctorSigDuplicates.nonEmpty)
        throw Error.Error(ctorSigDuplicates.mkString("\n"),
          "A class must not declare two constructors with the same parameter types",
          Error.Type.MethodsPass, Some(Error.Location(cd.name.row, cd.name.col, ast.fileName)))
    }

    super.visit(cd)
  }

  // An interface must not declare two methods with the same signature
  override def visit(id: InterfaceDecl): Unit = {
    val methodSigDuplicates = myMethods.keys.groupBy(identity).collect { case (x, List(_, _, _*)) => x }
    if(methodSigDuplicates.nonEmpty)
      throw Error.Error(methodSigDuplicates.mkString("\n"),
        "A class or interface must not declare two methods with the same signature",
        Error.Type.MethodsPass, Some(Error.Location(id.name.row, id.name.col, ast.fileName)))

    super.visit(id)
  }
}
