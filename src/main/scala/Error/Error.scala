package Error

import Token.Identifier

case object Type extends Enumeration {
  val CommandLine, Lexer, LiteralDFA, Parser, Weeder, ModifiersPass, ASTBuilder, TypeLinking, Disambiguation,
  Implementation, EnvironmentPass, ExtendsPass, HierarchyCheck, MethodsPass = Value
}

case class Location ( lineNum: Integer, col: Integer, file: String )

case class Error ( cause: String, msg: String, kind: Type.Value, loc: Option[Location] = None ) extends Exception

object Error {
  def identifierNotInScope(id: String): Error = {
    Error(id, "Could not find variable in scope", Type.Disambiguation)
  }

  def multipleTypes(id: String): Error = {
    Error(id, "Multiple instances of type declared", Type.TypeLinking)
  }

  def memberNotFound(typeId: String, member: Identifier): Error = {
    Error(member.lexeme, s"${member.lexeme} is not a member of $typeId", Type.Disambiguation)
  }

  def ambiguousName(id: String): Error = {
    Error(id, "Could not resolve", Type.Disambiguation)
  }

  def nonStaticAccess(typeId: String, member: Identifier): Error = {
    Error(member.lexeme, s"Trying to access non-static member of $typeId", Type.Disambiguation)
  }

  def noTopLevelPackage(id: String): Error = {
    Error(id, s"No top level package $id found", Type.Disambiguation)
  }

  def classNotFound(id: String): Error = {
    Error(id, s"Class $id was not found", Type.TypeLinking)
  }

  def langLibraryNotLoaded: Error = {
    Error("Compiler error", "Java default library was not properly loaded", Type.Implementation)
  }

  def undefinedMatch: Error = {
    Error("Compiler error", "This case should not have been matched", Type.Implementation)
  }
}
