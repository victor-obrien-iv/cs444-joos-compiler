package AST

import Token.Identifier
import scala.language.implicitConversions

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

  /**
    * @return The String representation of the Qualifier e.g. java.util.String
    */
  def name: String = (qualifiers.map(_.lexeme) :+ id.lexeme).mkString(".")
  /**
    * @return The String representation of the Qualifier excluding the id e.g. java.util
    */
  def pack: String = qualifiers.map(_.lexeme).mkString(".")
}

object FullyQualifiedID {
  /**
    * Builds a new FullyQualifedId from the list of qualifiers
    *
    * @param qualifiers Previous list in a FullyQualifiedId
    * @return A new FullyQualifiedId
    */
  def apply(qualifiers: List[Identifier]): FullyQualifiedID =
    FullyQualifiedID(qualifiers.dropRight(1), qualifiers.last)

  /**
    * Builds a FullyQualifiedId from a single identifier
    * @param id the identifier
    * @return A new FullyQualifiedId
    */
  def apply(id: Identifier): FullyQualifiedID = FullyQualifiedID(Nil, id)

  /**
    * Builds a FullyQualifiedId from a string
    * @param id the string representation of the id
    * @return A new FullyQualifiedID
    */
  def apply(id: String): FullyQualifiedID = {
    val idNames = id.split(".").toList
    val ids = idNames.map(Identifier(_, 0, 0))
    FullyQualifiedID(ids)
  }

  implicit def toName(id: FullyQualifiedID): String = id.name
}
