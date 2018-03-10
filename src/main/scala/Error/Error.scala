package Error

import AST.FullyQualifiedID
import Environment.{AugmentedNode, ExprAugmented, NamedExprAugmented}
import Token.Identifier

case object Type extends Enumeration {
  val CommandLine, Lexer, LiteralDFA, Parser, Weeder, ModifiersPass, ASTBuilder, TypeLinking, Disambiguation,
    EnvironmentPass, ExtendsPass, HierarchyCheck, MethodsPass = Value
}

case class Location ( lineNum: Integer, col: Integer, file: String )

case class Error ( cause: String, msg: String, kind: Type.Value, loc: Option[Location] = None ) extends Exception

object Error {
  def identifierNotInScope(id: FullyQualifiedID): Error = {
    Error(id.name, "Could not find variable in scope", Type.Disambiguation)
  }

  def multipleTypes(id: FullyQualifiedID): Error = {
    Error(id.name, "Multiple instances of type declared", Type.TypeLinking)
  }

  def memberNotFound(typeId: FullyQualifiedID, member: Identifier): Error = {
    Error(member.lexeme, s"${member.lexeme} is not a member of ${typeId.name}", Type.Disambiguation)
  }

  def ambiguousName(id: FullyQualifiedID): Error = {
    Error(id.name, "Could not resolve", Type.Disambiguation)
  }

  def nonStaticAccess(typeId: FullyQualifiedID, member: Identifier): Error = {
    Error(member.lexeme, s"Trying to acces non-static member of ${typeId.name}", Type.Disambiguation)
  }

  def noTopLevelPackage(id: FullyQualifiedID): Error = {
    Error(id.name, s"No top level package ${id.name} found", Type.Disambiguation)
  }
}
