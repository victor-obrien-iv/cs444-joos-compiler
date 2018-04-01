package StaticAnalyzer


import AST._

import scala.collection.mutable

class InitializationPass(fileName: String) extends Visitor {

  val declaredButNotInitialized: mutable.Set[String] = mutable.Set()

  override def visit(ds: DeclStmt): Unit = {
    declaredButNotInitialized += ds.decl.name.lexeme
    super.visit(ds)
    if(ds.assignment.nonEmpty)
      declaredButNotInitialized -= ds.decl.name.lexeme
  }

  override def visit(t: Type): Unit = {
    // stop types from being visited as type names may overlap with variable names
  }

  override def visit(dre: DeclRefExpr): Unit = {
    if(declaredButNotInitialized.contains(dre.reference.lexeme))
      throw Error.Error(dre.reference.lexeme, "Local variable used before assignment",
        Error.Type.StaticAnalysis, Some(Error.Location(dre.reference.row, dre.reference.col, fileName)))
  }

  override def visit(fqid: FullyQualifiedID): Unit = {
    if(fqid.qualifiers.isEmpty && declaredButNotInitialized.contains(fqid.id.lexeme))
      throw Error.Error(fqid.id.lexeme, "Local variable used before assignment",
        Error.Type.StaticAnalysis, Some(Error.Location(fqid.id.row, fqid.id.col, fileName)))
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
