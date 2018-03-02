package AST

/**
  * AstNode represents a node in the abstract syntax tree.
  * This is the base class for Stmt, Decl and Type, and it
  * does nothing on its own.
  */
trait AstNode

/**
  * FullyQualifiedID is a list of identifiers that specify what Decl the final id refers to.
  *   This is used when we can be certain that each qn in q1.q2.q3. ... .qn.id is a qualifier
  *   and not an object.  If we are uncertain, AccessExpr is used instead
  * ex: myClass.myStaticField
  *     myPackage.myClass.myStaticField
  *     myVar
  * @param qualifiers A potentially empty list of qualifiers leading up to id
  * @param id The identifier
  */
case class FullyQualifiedID(qualifiers: List[Token.Identifier], id: Token.Identifier) {
  def name: String = (qualifiers.map(_.lexeme) :+ id.lexeme).mkString(".")
}
