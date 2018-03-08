package Environment

import AST.FullyQualifiedID

/**
  * Type represents a data type
  * With the exception of primitives, each Type is the application of a Decl
  */
sealed trait TypeAugmented extends AugmentedNode

/**
  * ArrayType represents an array data type
  * ex: int[]
  * @param arrayOf The type of the elements in the array
  * @param size The expression that denotes the size of the array, if present
  */
case class ArrayTypeAugmented(arrayOf: TypeAugmented, size: Option[ExprAugmented], environment: Environment) extends TypeAugmented

/**
  * PrimitiveType represents an implicit primitive data type
  * ex: int
  * @param typeToken The token that denotes this primitive
  */
case class PrimitiveTypeAugmented(typeToken: Token.Primitive) extends TypeAugmented

/**
  * ClassType represents a non-primitive type defined as a class
  * @param typeID The id that denotes the class
  */
case class ClassTypeAugmented(typeID: FullyQualifiedID) extends TypeAugmented
