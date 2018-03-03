package Weeder

import AST._

import scala.collection.mutable.ListBuffer

/**
  * A pass to check the following:
  *   No two fields declared in the same class may have the same name
  *   No two local variables with overlapping scope have the same name
  *
  * this pass analyses:
  *   FieldDecl
  *   BlockStmt
  *   VarDecl
  *   ParameterDecl
  *
  */
class EnvironmentPass(val fileName: String) extends Visitor {

  // No two fields declared in the same class may have the same name
  val fieldNames: ListBuffer[String] = ListBuffer()
  override def visit(fd: FieldDecl): Unit = {
    if ( fieldNames.contains(fd.name.lexeme) )
      throw Error.Error(fd.name.lexeme, "No two fields declared in the same class may have the same name",
        Error.Type.EnvironmentPass, Some( Error.Location(fd.name.row, fd.name.col, fileName)))

    fieldNames += fd.name.lexeme
  }

  // No two local variables with overlapping scope have the same name
  val scopesStack: ListBuffer[ListBuffer[String]] = ListBuffer()
  def isInScope(name: String): Boolean = {
    for(s: ListBuffer[String] <- scopesStack; n: String <- s) {
      if ( n == name ) return true
    }
    false
  }

  override def visit(bs: BlockStmt): Unit = {
    scopesStack. +=: ( ListBuffer() ) // push an empty scope to the top of the stack
    super.visit(bs)
    scopesStack.remove(0)
  }

  override def visit(vd: VarDecl): Unit = {
    if ( isInScope(vd.name.lexeme) )
      throw Error.Error(vd.name.lexeme, "No two local variables with overlapping scope may have the same name",
        Error.Type.EnvironmentPass, Some( Error.Location(vd.name.row, vd.name.col, fileName)))
    scopesStack.head += vd.name.lexeme
  }

  override def visit(cd: ConstructorDecl): Unit = {
    scopesStack. +=: ( ListBuffer() ) // push an empty scope to the top of the stack
    super.visit(cd)
    scopesStack.remove(0)
  }

  override def visit(md: MethodDecl): Unit = {
    scopesStack. +=: ( ListBuffer() ) // push an empty scope to the top of the stack
    super.visit(md)
    scopesStack.remove(0)
  }

  override def visit(pd: ParameterDecl): Unit = {
    if ( isInScope(pd.name.lexeme) )
      throw Error.Error(pd.name.lexeme, "No two local variables with overlapping scope may have the same name",
        Error.Type.EnvironmentPass, Some( Error.Location(pd.name.row, pd.name.col, fileName)))
    scopesStack.head += pd.name.lexeme
  }
}
