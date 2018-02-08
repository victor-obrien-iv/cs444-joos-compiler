package Lalr

case class Lalr(terminals: Set[String], nonTerminals: Set[String],
           productionRules: Array[ProdRule], startSymbol: String, states: Int, actions: Map[(Int, String), Action])
  extends Serializable

sealed trait Action

case class Shift(nextState: Int) extends Action
case class Reduce(prodRule: Int) extends Action

case class ProdRule(nonTerminal: String, symbols: List[String])