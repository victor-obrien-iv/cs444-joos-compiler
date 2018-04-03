package Assembler

import AST.{BlockStmt, DeclStmt, Visitor}

class VarDeclCounter extends Visitor {
  var count = 0
  override def visit(ds: DeclStmt): Unit = count += 1
  def getNumVarDecl(bs: BlockStmt): Int = {
    count = 0
    visit(bs)
    count
  }
}
