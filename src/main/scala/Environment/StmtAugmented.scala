package Environment

/**
  * Stmt represents a code statement
  */
trait StmtAugmented extends AugmentedNode

/**
  * BlockStmt represents a sequence of statements
  * ex: { s1; s2; }
  * @param stmts The statements in this block
  */
case class BlockStmtAugmented(stmts: List[StmtAugmented]) extends StmtAugmented

/**
  * DeclStmt represents the declaration of a variable
  * ex: int i;
  * @param decl the declaration
  */
case class DeclStmtAugmented(decl: VarDeclAugmented, assignment: Option[ExprAugmented]) extends StmtAugmented

/**
  * ExprStmt represents the use of an expression as a statement
  * @param expr the expression
  */
case class ExprStmtAugmented(expr: ExprAugmented) extends StmtAugmented

/**
  * ReturnStmt represents a return statement
  * @param expr the expression to return or None for a void return
  */
case class ReturnStmtAugmented(expr: Option[ExprAugmented]) extends StmtAugmented

/**
  * CtrlFlowStmt qualifies statements that involve branching
  */
sealed trait CtrlFlowStmtAugmented extends StmtAugmented

/**
  * IfStmt represents an if-else stmt
  * @param condition An expression that evaluates to a boolean
  * @param thenStmt The stmt to be executed if condition is true
  * @param elseStmt The stmt, if any, to be executed if condition is false
  */
case class IfStmtAugmented(condition: ExprAugmented, thenStmt: StmtAugmented, elseStmt: Option[StmtAugmented],
                           environment: Environment) extends CtrlFlowStmtAugmented

/**
  * LoopStmt qualifies control flow statements that loop
  */
sealed trait LoopStmtAugmented extends CtrlFlowStmtAugmented

/**
  * ForStmt represents a for-loop
  * @param init The declarations for the for-loop
  * @param condition An expression that evaluates to a boolean, to be checked each time
  *                  before starting the body
  * @param update The statement run at the end of the body before checking condition again
  * @param bodyStmt The statement to be run each loop iteration
  */
case class ForStmtAugmented(init: Option[StmtAugmented], condition: Option[ExprAugmented],
                            update: Option[StmtAugmented], bodyStmt: StmtAugmented,
                            environment: Environment) extends LoopStmtAugmented

/**
  * WhileStmt represents a while-loop
  * @param condition An expression that evaluates to a boolean, to be checked each time
  *                  before starting the body
  * @param bodyStmt The statement to be each time condition evaluates to true
  */
case class WhileStmtAugmented(condition: ExprAugmented, bodyStmt: StmtAugmented,
                              environment: Environment) extends LoopStmtAugmented

