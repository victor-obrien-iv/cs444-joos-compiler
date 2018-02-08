package Parser

import Lalr.Lalr
import Token._

class Parser(lalr: Lalr) {

  //  def parse(tokens: List[Token]): ParseTreeNode = parseRec(tokens, Nil, Empty())

  def parseRec(tokens: List[Token], stack: List[(Int, String)]): Boolean = tokens match {
    case Nil => if (stack.head._2 == lalr.startSymbol) true else throw ParseError("Unexpected end of input")
    case head :: tail =>
      val state = if (stack.isEmpty) 0 else stack.head._1

      val action = lalr.actions(state, kind(head))
      action match {
        case Shift(symbol, nextState) => parseRec(tail, (nextState, symbol) :: stack)
        case Reduce(_, prodRule) => parseRec(tokens, reduce(lalr.productionRules(prodRule), stack))
      }

  }

  def reduce(prodRule: ProdRule, stack: List[(Int, String)]): List[(Int, String)] = {
    prodRule match {
      case ProdRule(nonTerminal, Nil) =>
        val action = lalr.actions((stack.head._1, nonTerminal))
        action match {
          case Shift(symbol, nextState) => (nextState, symbol) :: stack
          case Reduce(_, rule) => reduce(lalr.productionRules(rule), stack)
        }

      case ProdRule(nonTerminal, _ :: tail) => reduce(ProdRule(nonTerminal, tail), stack.tail)
    }
  }

  def kind(token: Token): String = token match {
    case _: Identifier => "Identifier"
    case _: Keyword => token.lexeme
    case _: BooleanLiteral => "BooleanLiteral"
    case _: IntegerLiteral => "IntegerLiteral"
    case _: CharacterLiteral => "CharacterLiteral"
    case _: NullLiteral => "NullLiteral"
    case _: StringLiteral => "StringLiteral"
    case _: Separator => token.lexeme
    case _: Operator => token.lexeme
  }
}
