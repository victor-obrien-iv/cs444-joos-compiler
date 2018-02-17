package AST

/**
  * Type represents a data type
  * With the exception of primitives, each Type is the application of a Decl
  */
sealed trait Type extends AstNode

/**
  * ArrayType represents an array data type
  * ex: int[]
  * @param arrayOf The type of the elements in the array
  * @param size The expression that denotes the size of the array, if present
  */
case class ArrayType(arrayOf: Type, size: Option[Expr]) extends Type

/**
  * PrimitiveType represents an implicit primitive data type
  * ex: int
  * @param typeToken The token that denotes this primitive
  */
case class PrimitiveType(typeToken: Token.Primitive) extends Type

/**
  * ClassType represents a non-primitive type defined as a class
  * @param typeID The id that denotes the class
  */
case class ClassType(typeID: FullyQualifiedID) extends Type
