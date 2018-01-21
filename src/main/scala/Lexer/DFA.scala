package Lexer

import akka.actor.Actor

import scala.collection.immutable

case class Result( lexeme: String = "", token: String = "ERROR" /*TODO: replace with token class*/)

case class EOF()
//case class Reset()

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
  var lastAcceptState: Option[Result] = Some( Result() )
  var currentState: state
  val startState: state

  val acceptingStates: Map[state, String /*TODO: replace with token class*/]
  val transitions: Map[(state, Char), state]

  def run( c: Char ): Option[Result] = {
    val nextState: Option[state] = transitions get ( currentState, c )

    // if we got none we are in the error state
    if ( nextState.isEmpty ) {
      val retval = lastAcceptState
      currentState = startState
      lastAcceptState = Some( Result() )
      return retval
    }

    // otherwise move to the new state
    currentState = nextState.get

    // this state may be an accepted one
    if ( acceptingStates.contains( currentState ) )
      // this being max munch we need to know how many chars along we are
      lastAcceptState = Some( Result( Lexer.lexeme, acceptingStates.apply( currentState ) ) )

    // return none to indicate we can handle more input
    None
  }


}


