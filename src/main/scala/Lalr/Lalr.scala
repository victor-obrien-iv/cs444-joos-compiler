package Lalr

import Parser.{Action, ProdRule}

case class Lalr(terminals: Set[String], nonTerminals: Set[String],
           productionRules: Array[ProdRule], startSymbol: String, states: Int, actions: Map[(Int, String), Action])
  extends Serializable
