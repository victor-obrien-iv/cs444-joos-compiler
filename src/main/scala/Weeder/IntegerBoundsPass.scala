package Weeder

import AST._
import Token._
import akka.actor.ActorRef

/**
  * A pass to check the following:
  *   The bounds of integer Literals
  *   That 2147483648 only appears in front of a unary minus operator
  *
  * this pass analyses:
  *   UnaryExpr
  *   ValExpr
  */
class IntegerBoundsPass(val fileName: String, val reporter: ActorRef) extends Visitor {
  val twoPow31: BigInt = 2147483648L
  val intMax: BigInt = 2147483647

  // 2147483648, can only appear in front of a unary minus operator
  override def visit(ue: UnaryExpr): Unit = {
    ue.rhs match {
      case ValExpr(value: Token.Literal) =>
        value match {
          case IntegerLiteral(_, _, _, value: BigInt) =>
            if( value <= twoPow31 ) return
          case _ =>
        }
      case _ =>
    }

    super.visit(ue: UnaryExpr)
  }

  // The bounds of integer Literals
  override def visit(ve: ValExpr): Unit = {
    ve.value match {
      case IntegerLiteral(lexeme, row, col, value: BigInt) =>
        assert(value >= 0, "No integer literal should have been saved as a negative")
        if( value > intMax )
          reporter ! Error.Error(lexeme,
            "An integer literal cannot exceed 2147483647",
            Error.Type.Weeder, Some( Error.Location(row, col, fileName)))
      case _ =>
    }
  }
}
