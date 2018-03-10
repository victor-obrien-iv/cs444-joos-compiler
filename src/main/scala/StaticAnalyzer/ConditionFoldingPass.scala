package StaticAnalyzer

import AST._

class ConditionFoldingPass(fileName: String) extends Visitor {
  case class UnreachableCodeException() extends Exception
  case class IndefiniteLoopException(ls: LoopStmt) extends Exception

  override def visit(td: TypeDecl): Unit = {
    try super.visit(td)
    catch {
      case _: UnreachableCodeException =>
        throw Error.Error(td.name.lexeme, "Unreachable code found in body",
          Error.Type.StaticAnalysis, Some(Error.Location(td.name.row, td.name.col, fileName)))
    }
  }

  override def visit(bs: BlockStmt): Unit = {
    try super.visit(bs)
    catch {
      case e: IndefiniteLoopException =>
        if(e.ls != bs.stmts.last) throw UnreachableCodeException()

    }
  }

  override def visit(ls: LoopStmt): Unit = {
    val hasConstCondition = ls match {
      case ForStmt(_, condition, _, _) => condition match {
        case Some(expr) => Expr.tryFoldBoolean(expr)
        case None => Some(true) // if a for loop has an empty condition, it always evaluates to true
      }
      case WhileStmt(condition, _) => Expr.tryFoldBoolean(condition)
    }

    hasConstCondition match {
      case Some(evaluatesTo) =>
        if(evaluatesTo)
          throw IndefiniteLoopException(ls) // any statements after this loop won't be reached
        else
          throw UnreachableCodeException() // the body of the loop is unreachable if the condition is always false
      case None =>
    }

    super.visit(ls)
  }

  override def visit(is: IfStmt): Unit = {
    val hasConstCondition = Expr.tryFoldBoolean(is.condition)
    hasConstCondition match {
      case Some(evaluatesTo) =>
        if((evaluatesTo && is.elseStmt.isDefined) // the else statements cannot be reached
          || !evaluatesTo) // the then statements cannot be reached
          throw UnreachableCodeException()
      case None =>
    }
    super.visit(is)
  }

}
