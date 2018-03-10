package StaticAnalyzer

import AST._

class ReachabilityPass(fileName: String) extends Visitor {
  case class UnreachableCodeException() extends Exception

  override def visit(md: MethodDecl): Unit = md.body match {
    case Some(body) =>
      def err(msg: String) =
        throw Error.Error(md.name.lexeme, msg,
          Error.Type.ReachabilityPass, Some(Error.Location(md.name.row, md.name.col, fileName)))

      try
        if(!returns(body) && md.returnType.isDefined)
          err("Method whose return type is not void does not return on all paths")

      catch {
        case _: UnreachableCodeException =>
          err("Unreachable code found in method body")
      }
    case None =>
  }

  override def visit(cd: ConstructorDecl): Unit = {
    try returns(cd.body)
    catch {
      case _: UnreachableCodeException =>
        throw Error.Error(cd.identifier.lexeme, "Unreachable code found in constructor declaration",
          Error.Type.ReachabilityPass, Some(Error.Location(cd.identifier.row, cd.identifier.col, fileName)))
    }
  }

  /**
    * @return true iff the stmt will return on all its paths
    * @throws UnreachableCodeException if stmt includes unreachable code
    */
  def returns(stmt: Stmt): Boolean = stmt match {
    case bs: BlockStmt => returns(bs)
    case _: DeclStmt => false
    case _: ExprStmt => false
    case _: ReturnStmt => true
    case cfs: CtrlFlowStmt => returns(cfs)
  }

  def returns(bs: BlockStmt): Boolean = {
    val stmts = bs.stmts

    def firstReturnIndex: Int = {
      for(i <- stmts.indices)
        if(returns(stmts(i))) return i
      stmts.size
    }
    val i = firstReturnIndex

    if(i == stmts.size)
      false
    else {
      if(i < stmts.size - 1) throw UnreachableCodeException()
      true
    }
  }

  def returns(cfs: CtrlFlowStmt): Boolean = cfs match {
    case is: IfStmt => returns(is)
    case ls: LoopStmt => returns(ls.bodyStmt)
  }

  def returns(is: IfStmt): Boolean = {
    val thenReturns = returns(is.thenStmt)
    val elseReturns = is.elseStmt match {
      case Some(stmt) => returns(stmt)
      case None => false
    }
    thenReturns && elseReturns
  }

}
