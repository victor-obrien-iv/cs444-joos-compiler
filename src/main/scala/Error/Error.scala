package Error

import Token.{Identifier, Primitive}

case object Type extends Enumeration {
  val CommandLine, Lexer, LiteralDFA, Parser, Weeder, ModifiersPass, ASTBuilder, TypeLinking, Disambiguation,
  Implementation, TypeChecker, EnvironmentPass, ExtendsPass, HierarchyCheck, MethodsPass = Value
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

  def typeMismatch(actual: AST.Type, expected: AST.Type): Error = {
    Error(actual.toString, s"$actual is not type assignable to expected type $expected", Type.TypeChecker)
  }

  def expectedBoolean(actual: AST.Type): Error = {
    Error(actual.toString, s"$actual is not type assignable to expected type boolean", Type.TypeChecker)
  }

  def expectedNumeric(actual: AST.Type): Error = {
    Error(actual.toString, s"$actual is not numeric", Type.TypeChecker)
  }

  def notArray(actual: AST.Type): Error = {
    Error(actual.toString, s"$actual is not of type []", Type.TypeChecker)
  }

  def primitiveDoesNotContainField(primitive: Primitive, field: Identifier): Error = {
    Error(primitive.lexeme, s"$primitive cannot be called with $field", Type.TypeChecker)
  }

  def accessPrimitiveType(primitive: Primitive, method: Identifier): Error = {
    Error(primitive.lexeme, s"$primitive cannot be called with $method", Type.TypeChecker)
  }

  def nullPointerException: Error = {
    Error("null", "null cannot be called with accessor", Type.TypeChecker)
  }
}
