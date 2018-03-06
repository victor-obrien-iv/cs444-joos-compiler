package HierarchyChecker

import AST._

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

  private lazy val myMethods: Map[checker.MethodSig, MethodDecl]
    = checker.declaredMethods(ast.typeDecl)
  private lazy val myConstructors: Map[checker.ConstructorSig, ConstructorDecl]
    = checker.declaredConstructors(ast.typeDecl)

  override def visit(cd: ClassDecl): Unit = {
    super.visit(cd)
  }

  override def visit(id: InterfaceDecl): Unit = {
    super.visit(id)
  }
}
