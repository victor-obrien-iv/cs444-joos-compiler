package Parser

import Lalr.Lalr
import Token._

class Parser(lalr: Lalr) {

  def parse(tokens: List[Token]): TreeNode = parseRec((Bof()::tokens) :+ Eof(), Nil)

  def parseRec(tokens: List[Token], stack: List[(Int, TreeNode)]): TreeNode = tokens match {
    case Nil => reduce(lalr.productionRules(0), stack, Nil).head._2
    case head :: tail =>
      val state = if (stack.isEmpty) 0 else stack.head._1

      val action = lalr.actions(state, head.kind)
      action match {
        case Shift(symbol, nextState) => parseRec(tail, (nextState, TreeNode(symbol, Nil)) :: stack)
        case Reduce(_, prodRule) =>
          val newStack = reduce(lalr.productionRules(prodRule), stack, Nil)
          parseRec(tokens, newStack)
      }

  }

  def reduce(prodRule: ProdRule, stack: List[(Int, TreeNode)], children: List[TreeNode]): List[(Int, TreeNode)] = {
    prodRule match {
      case ProdRule(nonTerminal, Nil) =>
        val state = if (stack.isEmpty) 0 else stack.head._1

        if (nonTerminal == lalr.startSymbol) {
          (0, TreeNode(nonTerminal, children))::Nil
        } else {
          val action = lalr.actions((state, nonTerminal))
          action match {
            case Shift(symbol, nextState) => (nextState, TreeNode(symbol, children)) :: stack
            case Reduce(_, rule) => reduce(lalr.productionRules(rule), stack, Nil)
          }
        }
      case ProdRule(nonTerminal, _ :: tail) =>
        val node = stack.head._2
        reduce(ProdRule(nonTerminal, tail), stack.tail, node::children)
    }
  }
}
