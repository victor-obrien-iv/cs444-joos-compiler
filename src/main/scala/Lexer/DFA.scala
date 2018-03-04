package Lexer

import Token.Token

import scala.collection.immutable

case class EOF()

object DFA {
  val whitespace = "\t\n\r "
  val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  val digits = "0123456789"
  val alphanumeric: String = letters + digits
  val javaLetters: String = letters + "_$"
  val hexDigits = "0123456789ABCDEFabcdef"
  val octDigits = "01234567"
  val oneToNine =  "123456789"
  val escapeChars = "btnfr\'\"\\"
  val allAscii: immutable.IndexedSeq[Char] = ( for (i <- 33 to 126 ) yield i.toChar ) + whitespace
}

abstract class DFA[state](status: Status) {

  var lastToken: Option[Token.Token] = None
  var currentState: state
  val startState: state
  val acceptingStates: Map[state, () => Token.Token]
  val transitions: Map[(state, Char), state]

  def run( c: Char ): Option[Option[Token.Token]] = {
    val nextState: Option[state] = transitions get(currentState, c)

    nextState match {
      case None =>
        // we have hit an error state, return the last accept state
        val retVal = Some(lastToken)
        reset()
        retVal

      case Some(s) =>
        // otherwise move to the new state
        currentState = nextState.get

        // this state may be an accepted one
        if (acceptingStates.contains(currentState)) {
          val tokenCtor: () => Token = acceptingStates.apply(currentState)
          lastToken = Some(tokenCtor())
        }

        // return none to indicate we can handle more input
        None
    }
  }

  def getLastToken: Option[Option[Token]] = {
    Some(lastToken)
  }

  def reset(): Unit = {
    lastToken = None
    currentState = startState
  }
}


