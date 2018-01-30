package Lexer

import akka.actor.Actor

import scala.collection.immutable

case class EOF()

object DFA {
  val whitespace = "\t\n\r "
  val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  val digits = "0123456789"
  val alphanumeric: String = letters + digits
  val hexDigits = "0123456789ABCDEFabcdef"
  val oneToNine =  "123456789"
  val allAscii: immutable.IndexedSeq[Char] = for (i <- 32 to 126 ) yield i.toChar
}

abstract class DFA[state] extends Actor {
  var lastToken: Option[Token.Token] = None
  var currentState: state
  val startState: state

  val acceptingStates: Map[state, (String, Int, Int) => Token.Token]
  val transitions: Map[(state, Char), state]

  def run( c: Char ): Option[Option[Token.Token]] = {
    val nextState: Option[state] = transitions get(currentState, c)

    nextState match {
      case None =>
        // we have hit an error state, return the last accept state
        currentState = startState
        val retVal = Some(lastToken)
        lastToken = None

        retVal

      case Some(s) =>
        // otherwise move to the new state
        currentState = nextState.get

        // this state may be an accepted one
        if (acceptingStates.contains(currentState)) {
          // this being max munch we need to know how many chars along we are

          val tokenCtor = acceptingStates.apply(currentState)
          lastToken = Some(tokenCtor(Lexer.state.getLexeme, Lexer.state.getRow, Lexer.state.getCol))
        }

        // return none to indicate we can handle more input
        None
    }
  }
}


