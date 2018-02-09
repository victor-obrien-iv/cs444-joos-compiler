package Parser

sealed trait Action

case class Shift(symbol: String, nextState: Int) extends Action
case class Reduce(symbol: String, prodRule: Int) extends Action

case class ProdRule(nonTerminal: String, symbols: List[String])