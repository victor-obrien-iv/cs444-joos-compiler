package StaticAnalyzer


import AST._

import scala.collection.mutable

class InitializationBeforeUsePass(fileName: String) extends Visitor {

  val declaredButNotInitialized: mutable.Set[String] = mutable.Set()

  override def visit(ds: DeclStmt): Unit = {
    super.visit(ds)
    if(ds.assignment.isEmpty) declaredButNotInitialized += ds.decl.name.lexeme
  }

  override def visit(dre: DeclRefExpr): Unit = {
    if(declaredButNotInitialized.contains(dre.reference.lexeme))
      throw Error.Error(dre.reference.lexeme, "Local variable used before assignment",
        Error.Type.StaticAnalysis, Some(Error.Location(dre.reference.row, dre.reference.col, fileName)))
  }

  override def visit(be: BinaryExpr): Unit = be.lhs match {
    case DeclRefExpr(reference) =>
      if(be.operatorTok.isInstanceOf[Token.Becomes]) {
        visit(be.rhs)
        declaredButNotInitialized -= reference.lexeme
      }
      else
        super.visit(be)
    case _ =>
      super.visit(be)
  }



}
