package AST

/**
  * Stmt represents a code statement
  */
trait Stmt extends AstNode

/**
  * BlockStmt represents a sequence of statements
  * ex: { s1; s2; }
  * @param stmts The statements in this block
  */
case class BlockStmt(stmts: List[Stmt]) extends Stmt

/**
  * DeclStmt represents the declaration of a variable
  * ex: int i;
  * @param decl the declaration
  */
case class DeclStmt(decl: VarDecl, assignment: Option[Expr]) extends Stmt

/**
  * ExprStmt represents the use of an expression as a statement
  * @param expr the expression
  */
case class ExprStmt(expr: Expr) extends Stmt

/**
  * ReturnStmt represents a return statement
  * @param expr the expression to return or None for a void return
  */
case class ReturnStmt(expr: Option[Expr]) extends Stmt

/**
  * CtrlFlowStmt qualifies statements that involve branching
  */
sealed trait CtrlFlowStmt extends Stmt

/**
  * IfStmt represents an if-else stmt
  * @param condition An expression that evaluates to a boolean
  * @param thenStmt The stmt to be executed if condition is true
  * @param elseStmt The stmt, if any, to be executed if condition is false
  */
case class IfStmt(condition: Expr, thenStmt: Stmt, elseStmt: Option[Stmt]) extends CtrlFlowStmt

/**
  * LoopStmt qualifies control flow statements that loop
  */
sealed trait LoopStmt extends CtrlFlowStmt {
  def bodyStmt: Stmt
}

/**
  * ForStmt represents a for-loop
  * @param init The declarations for the for-loop
  * @param condition An expression that evaluates to a boolean, to be checked each time
  *                  before starting the body
  * @param update The statement run at the end of the body before checking condition again
  * @param bodyStmt The statement to be run each loop iteration
  */
case class ForStmt(init: Option[Stmt], condition: Option[Expr],
                   update: Option[Stmt], bodyStmt: Stmt) extends LoopStmt

/**
  * WhileStmt represents a while-loop
  * @param condition An expression that evaluates to a boolean, to be checked each time
  *                  before starting the body
  * @param bodyStmt The statement to be each time condition evaluates to true
  */
case class WhileStmt(condition: Expr, bodyStmt: Stmt) extends LoopStmt

