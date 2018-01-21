package Lexer

object LiteralDFA extends Enumeration {
  val START = Value

  // states used for comments
  object comment {
    val FWD_SLASH, FWD_SLASH2, SINGLE, NEWLINE,
        STAR, STAR2, FWD_SLASH3, MULTI = Value
  }

  // states used for string literals
  object strLit {

  }
}

class LiteralDFA extends DFA[LiteralDFA.Value] {
  import LiteralDFA._

  val startState = START
  var currentState = startState

  val acceptingStates = Map(
    comment.SINGLE      -> "COMMENT",
    comment.NEWLINE     -> "COMMENT",
    comment.FWD_SLASH3  -> "COMMENT"
  )

  val transitions =
  // transitions for comments
  // TODO: if java 2 doesn't allow embedded comments this will need to be updated
  Map (
      (START, '/')              -> comment.FWD_SLASH, // /
      (comment.FWD_SLASH, '/')  -> comment.SINGLE,    // //
    ) ++
      ( DFA.allAscii filterNot '\n'.== ).map( c =>    // // fooBAR! 123
      (comment.SINGLE, c)       -> comment.SINGLE     ).toMap ++
  Map (
      (comment.SINGLE, '\n')    -> comment.NEWLINE,   // // fooBAR! 123 \n
      (comment.FWD_SLASH, '*')  -> comment.MULTI,     // /*
    ) ++
      ( DFA.allAscii filterNot '*'.== ).map( c =>     // /* fooBAR! 123
      (comment.MULTI, c)        -> comment.MULTI      ).toMap ++
  Map (
      (comment.MULTI, '*')      -> comment.STAR2,     // /* foo *
      (comment.STAR2, '*')      -> comment.STAR2,     // /* foo **
    ) ++
      ( DFA.allAscii filterNot '/'.== ).map( c =>     // /* foo *** bar
      (comment.STAR2, c)        -> comment.MULTI      ).toMap ++
  Map (
      (comment.STAR2, '/')      -> comment.FWD_SLASH3 // /* foo */
    ) //++
  // transitions for string literals


  def receive: PartialFunction[Any, Unit] = {
    case c: Char => sender() ! run(c)
    case EOF() => sender() ! lastAcceptState
//    case Reset => {
//      currentState = START
//      lastAcceptState = Some( Result() )
//    }
  }
}
