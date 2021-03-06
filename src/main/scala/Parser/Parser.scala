package Parser

import Lalr.Lalr
import Token._

import scala.annotation.tailrec

class Parser(lalr: Lalr, filename: String) {
  var errorLoc: Option[Error.Location] = None
  var errorLex: String = ""

  /**
    * Parses an augmented version of the given tokens
    *
    * @param tokens Tokens to be parsed
    * @return Parse Tree
    */
  //TODO: add EOF to lexer to record line number
  def parse(tokens: List[Token]): TreeNode = {
    try {
      parseRec((Bof() :: tokens) :+ Eof(), Nil)
    }
    catch {
      case _: NoSuchElementException =>
        throw Error.Error(errorLex, "illegal transition", Error.Type.Parser, errorLoc)
    }
  }

  @tailrec private def parseRec(tokens: List[Token], stack: List[(Int, TreeNode)]): TreeNode = tokens match {
    //Base case: No tokens to be read
    case Nil =>
      reduce(lalr.productionRules(0), stack, Nil).head._2
    case head :: tail =>
      val state = if (stack.isEmpty) 0 else stack.head._1

      errorLoc = Some(Error.Location(head.row, head.col, filename))
      errorLex = head.lexeme
      val action = lalr.actions(state, head.kind)
      action match {
        case Shift(symbol, nextState) => parseRec(tail, (nextState, TreeNode(Left(head), Nil)) :: stack)
        case Reduce(_, prodRule) =>
          val newStack = reduce(lalr.productionRules(prodRule), stack, Nil)
          parseRec(tokens, newStack)
      }

  }

  /**
    * Reduce pops items off the stack according to the production rule, accumulates parse tree nodes and pushes
    * on the non-terminal to the stack
    *
    * @param prodRule The production rule used for reduction
    * @param stack    The stack of states and nodes keeping track of parsing
    * @param children The children accumulated to be added to the node once reduced
    * @return The new stack
    */
  @tailrec private def reduce(prodRule: ProdRule, stack: List[(Int, TreeNode)], children: List[TreeNode]): List[(Int, TreeNode)] = {
    prodRule match {
      //Base case: Production rule is empty
      case ProdRule(nonTerminal, Nil) =>
        //Check if in start state
        val state = if (stack.isEmpty) 0 else stack.head._1

        if (nonTerminal == lalr.startSymbol) {
          (0, TreeNode(Right(nonTerminal), children)) :: Nil
        } else {
          val action = lalr.actions((state, nonTerminal))
          action match {
            case Shift(symbol, nextState) => (nextState, TreeNode(Right(nonTerminal), children)) :: stack
            case Reduce(_, rule) => reduce(lalr.productionRules(rule), stack, children)
          }
        }
      case ProdRule(nonTerminal, _ :: tail) =>
        val node = stack.head._2
        reduce(ProdRule(nonTerminal, tail), stack.tail, node :: children)
    }
  }
}
