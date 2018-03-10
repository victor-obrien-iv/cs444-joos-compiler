package AST

import Token._

/**
  * An Expr is an expression statement that returns
  * some value or is void
  */
sealed trait Expr extends AstNode

/**
  * BinaryExpr represents a binary operator acting on a left and right side
  * @param lhs The left hand side expression
  * @param operatorTok The token denoting the operation
  * @param rhs The right hand side expression
  */
case class BinaryExpr(lhs: Expr, operatorTok: Token.Operator, rhs: Expr) extends Expr

/**
  * UnaryExpr represents a single operator acting on a right hand side
  * @param operatorTok The token denoting the operation
  * @param rhs The right hand side
  */
case class UnaryExpr(operatorTok: Token.Operator, rhs: Expr) extends Expr

/**
  * ParenExpr represents an expression enclosed in parentheses
  * @param expr The expression with the parentheses
  */
case class ParenExpr(expr: Expr) extends Expr
/**
  * CallExpr represents a function or method call
  * ex: myClass.myStaticFn(a)
  *     myObjArray[0].myFn(a)
  *     myStaticFn(a)
  * @param obj The expr which results in the object/class on which the call
  *            is to be made or None in the case of a static call without any
  *            qualifiers
  * @param call The token that denotes the function to call
  * @param params The parameters for the function call
  */
case class CallExpr(obj: Option[Expr], call: Token.Identifier, params: List[Expr]) extends Expr

/**
  * ThisExpr represents "this" in the source code
  */
case class ThisExpr() extends Expr

/**
  * CastExpr represents a cast of the rhs to type castType
  * ex: (int)'a'
  * @param castType The type the rhs is being cast to
  * @param rhs The expression that returns some value to cast
  */
case class CastExpr(castType: Type, rhs: Expr) extends Expr

/**
  * FieldAccessExpr represents the accessing of a member of some object. This will
  *   be analyzed by the name resolver to determine what the field actually refers to
  *   based on lhs and map this Expr to the appropriate Decl
  * ex: myObjArray[0].myField
  *     myClass.myStaticField
  *     myObj.myField
  * @param lhs An expression that refers an object/class to be accessed
  * @param field An identifier that denotes the name of the member
  */
case class AccessExpr(lhs: Expr, field: Token.Identifier) extends Expr

/**
  * ArrayAccessExpr represents the accessing of an element in an array via an index
  * ex: myObjArray[0]
  *     (new int[5])[0]
  * @param lhs an expression that produces an array to be accessed
  * @param index an expression that evaluates to a number that will access into
  *              the array produced by the lhs
  */
case class ArrayAccessExpr(lhs: Expr, index: Expr) extends Expr

/**
  * ValExpr represents a compile-time known value written in code
  * ex: 5
  *     'b'
  *     "foo"
  * @param value The literal token this expr represents
  */
case class ValExpr(value: Token.Literal) extends Expr

/**
  * DeclRefExpr represents a reference to a declaration in an expression.
  *   The name resolution visitor will determine what decl this actually
  *   points to.
  * ex: myVar
  * @param reference The identifier of the variable being used
  */
case class DeclRefExpr(reference: Token.Identifier) extends Expr

/**
  * InstanceOfExpr represents a use of the 'instanceof' operator.
  * @param lhs an expression that evaluates to an object
  * @param typ the type the operator is checking against
  */
case class InstanceOfExpr(lhs: Expr, typ: Type) extends Expr

/**
  * NewExpr qualifies expressions that allocate memory on the heap
  */
sealed trait NewExpr extends Expr

/**
  * ObjectNewExpr represents new being called on a class constructor
  * @param ctor the identifier that specifies the constructor to use
  * @param params The list of parameters for the constructor
  */
case class ObjNewExpr(ctor: FullyQualifiedID, params: List[Expr]) extends NewExpr

/**
  * ArrayNewExpr represents new being called to allocate an array on the heap
  * ex: new int[5]
  * @param arrayType The type of the elements in the array
  */
case class ArrayNewExpr(arrayType: ArrayType) extends NewExpr

/**
  * Named expression for variable and field references
  * @param name The name of the field
  */
case class NamedExpr(name: FullyQualifiedID) extends Expr

object Expr {
  case class CouldNotFold(expr: Expr) extends Exception

  /**
    * @return true if e is a constant expression that evaluates to true,
    *         false if e is a constant expression that evaluates to false,
    *         None if e is not a constant expression that evaluates to a boolean
    */
  def tryFoldBoolean(expr: Expr): Option[Boolean] = {
    try Some(getAsBoolean(expr))
    catch {
      case _: CouldNotFold => None
      case _: ArithmeticException => None
    }
  }

  def tryFoldInteger(expr: Expr): Option[Int] = {
    try Some(getAsNumber(expr))
    catch {
      case _: CouldNotFold => None
      case _: ArithmeticException => None
    }
  }

  def getAsBoolean(expr: Expr): Boolean = expr match {
    case BinaryExpr(lhs, operatorTok, rhs) => operatorTok match {
      case _: GT =>
        getAsNumber(lhs) > getAsNumber(rhs)
      case _: LT =>
        getAsNumber(lhs) < getAsNumber(rhs)
      case _: EQ =>
        try getAsNumber(lhs) == getAsNumber(rhs)
        catch {
          case _: CouldNotFold =>
            getAsBoolean(lhs) == getAsBoolean(rhs)
        }
      case _: GE =>
        getAsNumber(lhs) >= getAsNumber(rhs)
      case _: LE =>
        getAsNumber(lhs) <= getAsNumber(rhs)
      case _: NE =>
        try getAsNumber(lhs) != getAsNumber(rhs)
        catch {
          case _: CouldNotFold =>
            getAsBoolean(lhs) != getAsBoolean(rhs)
        }
      case _: AmpAmp =>
        getAsBoolean(lhs) && getAsBoolean(rhs)
      case _: BarBar =>
        getAsBoolean(lhs) || getAsBoolean(rhs)
      case _ =>
        throw CouldNotFold(expr)
    }
    case UnaryExpr(operatorTok, rhs) => operatorTok match {
      case _: Bang =>
        !getAsBoolean(rhs)
      case _ =>
        throw CouldNotFold(expr)
    }
    case ParenExpr(innerExpr) =>
      getAsBoolean(innerExpr)
    case CastExpr(castType, rhs) => castType match {
      case PrimitiveType(typeToken) => typeToken match {
        case _: JavaBoolean =>
          getAsBoolean(rhs)
        case _ =>
          throw CouldNotFold(expr)
      }
      case _ =>
        throw CouldNotFold(expr)
    }
    case ValExpr(value) => value match {
      case BooleanLiteral(_, _, boolean) =>
        boolean
      case _ =>
        throw CouldNotFold(expr)
    }
    case _ =>
      throw CouldNotFold(expr)
  }

  def getAsNumber(expr: Expr): Int = expr match {
    case BinaryExpr(lhs, operatorTok, rhs) =>
      val leftNum = getAsNumber(lhs)
      val rightNum = getAsNumber(rhs)
      operatorTok match {
        case _: Amp =>
          leftNum & rightNum
        case _: Bar =>
          leftNum | rightNum
        case _: Plus =>
          leftNum + rightNum
        case _: Minus =>
          leftNum - rightNum
        case _: Star =>
          leftNum * rightNum
        case _: Slash =>
          leftNum / rightNum
        case _: Percent =>
          leftNum % rightNum
        case _ =>
          throw CouldNotFold(expr)
      }
    case UnaryExpr(operatorTok, rhs) => operatorTok match {
      case _: Minus => rhs match {
        case literal: IntegerLiteral if !literal.value.isValidInt =>
          Integer.MIN_VALUE // handle -2,147,483,647 edge case
        case _ =>
          0 - getAsNumber(rhs)
      }
      case _ => throw CouldNotFold(expr)
    }
    case ParenExpr(innerExpr) =>
      getAsNumber(innerExpr)
    case CastExpr(castType, rhs) => castType match {
      case PrimitiveType(typeToken) =>
        val rightNum = getAsNumber(rhs)
        typeToken match {
          case _: JavaByte =>
            rightNum.toByte
          case _: JavaChar =>
            rightNum.toChar
          case _: JavaShort =>
            rightNum.toShort
          case _: JavaInt =>
            rightNum
          case _ =>
            throw CouldNotFold(expr)
        }
      case _ =>
        throw CouldNotFold(expr)
    }
    case ValExpr(value) => value match {
      case IntegerLiteral(_, _, _, int) =>
        assert(int.isValidInt, "Integer literal too big")
        int.toInt
      case CharacterLiteral(_, _, _, char) =>
        char.toInt
      case _ =>
        throw CouldNotFold(expr)
    }
    case _ =>
      throw CouldNotFold(expr)
  }
}
