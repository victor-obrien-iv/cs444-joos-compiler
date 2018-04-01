package AST

import Token._

/**
  * Type represents a data type
  * With the exception of primitives, each Type is the application of a Decl
  */
sealed trait Type extends AstNode

sealed trait ReferenceType extends Type
/**
  * ArrayType represents an array data type
  * ex: int[]
  * @param arrayOf The type of the elements in the array
  * @param size The expression that denotes the size of the array, if present
  */
case class ArrayType(arrayOf: Type, size: Option[Expr]) extends ReferenceType {
  def equal(arrayType: ArrayType): Boolean = arrayOf == arrayType.arrayOf
}

/**
  * PrimitiveType represents an implicit primitive data type
  * ex: int
  * @param typeToken The token that denotes this primitive
  */
case class PrimitiveType(typeToken: Token.Primitive) extends Type {
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  def equals(token: Token.Primitive): Boolean = token.lexeme == typeToken.lexeme

  def isNumeric: Boolean = {
    typeToken match {
      case JavaByte(_, _, _) | JavaChar(_, _, _) | JavaShort(_, _, _) | JavaInt(_, _, _) => true
      case _ => false
    }
  }
}

/**
  * Type for null literal
  */
case class NullType() extends ReferenceType

/**
  * ClassType represents a non-primitive type defined as a class
  * @param typeID The id that denotes the class
  */
case class ClassType(typeID: FullyQualifiedID) extends ReferenceType {
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  def equals(fullyQualifiedID: FullyQualifiedID): Boolean = typeID.name == fullyQualifiedID.name
}

/**
  * Actual class
  * @param typeId The id that denotes the class
  */
case class Class(typeId: FullyQualifiedID) extends ReferenceType
