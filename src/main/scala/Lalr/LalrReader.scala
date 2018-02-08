package Lalr

import java.io.{FileOutputStream, ObjectOutputStream, OutputStream}

import scala.io.Source

object LalrReader extends App {

  val lr1File = Source.fromFile("src/main/resources/joos.lr1").getLines().toArray

  val terminalEndIdx = lr1File(0).toInt + 1
  val terminals = lr1File.slice(1, terminalEndIdx).toSet

  val nonTerminalEndIdx = terminalEndIdx + lr1File(terminalEndIdx).toInt + 1
  val nonTerminals = lr1File.slice(terminalEndIdx + 1, nonTerminalEndIdx).toSet

  val startSymbol = lr1File(nonTerminalEndIdx)

  val prodRulesEndIdx = nonTerminalEndIdx + lr1File(nonTerminalEndIdx + 1).toInt + 2
  val prodRules = lr1File.slice(nonTerminalEndIdx + 2, prodRulesEndIdx)

  val states = lr1File(prodRulesEndIdx).toInt

  val actionsEndIdx = prodRulesEndIdx + lr1File(prodRulesEndIdx + 1).toInt + 2
  val actions = lr1File.slice(prodRulesEndIdx + 2, actionsEndIdx)

  val lalr = Lalr(terminals, nonTerminals, parseProdRules(prodRules), startSymbol, states, parseActions(actions))

  val out = new ObjectOutputStream(new FileOutputStream("src/main/resources/lalr"))

  out.writeObject()
  out.close()

  def parseProdRules(rules: Array[String]): Array[ProdRule] = rules map { rule: String =>
    rule.split(" ").toList match {
      case head :: rest => ProdRule(head, rest.reverse) //Reverse the rest to match while popping during parsing
    }
  }

  def parseActions(actions: Array[String]) = {
    val tupleArray = actions map { action: String =>
      action.split(" ").toList match {
        case List(state: String, symbol: String, shiftReduce: String, newState: String) => shiftReduce match {
          case "shift" => ((state.toInt, symbol), Shift(newState.toInt))
          case "reduce" => ((state.toInt, symbol), Reduce(newState.toInt))
        }
      }
    }
    tupleArray.toMap
  }
}
