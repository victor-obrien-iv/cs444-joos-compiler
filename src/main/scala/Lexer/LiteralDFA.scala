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
    val QUOTE, STR, ESC, QUOTE2, PRE_SPACE,
        PLUS, POST_SPACE = Value
  }

  // states for a char literal
  object char {
    val APOST, CHAR, ESC, APOST2 = Value
  }

  // states for an octal escape, ex: \064
  object octal {
    val DIGIT1, DIGIT2 = Value
  }
}

class LiteralDFA extends DFA[LiteralDFA.Value] {
  import LiteralDFA._

  val startState = START
  var currentState = startState

  val acceptingStates = Map(
    comment.SINGLE      -> Token.Comment.apply _,
    comment.NEWLINE     -> Token.Comment.apply _,
    comment.FWD_SLASH3  -> Token.Comment.apply _,
    strLit.QUOTE2       -> Token.StringLiteral.apply _
  )

  val commentTransitions: Map[(Value, Char), Value] =
    Map (
      (START, '/')              -> comment.FWD_SLASH, // /
      (comment.FWD_SLASH, '/')  -> comment.SINGLE,    // //
    ) ++
    (DFA.allAscii filterNot '\n'.== ).map( c =>       // // fooBAR! 123
      (comment.SINGLE, c)       -> comment.SINGLE ).toMap ++
    Map (
      (comment.SINGLE, '\n')    -> comment.NEWLINE,   // // fooBAR! 123 \n
      (comment.FWD_SLASH, '*')  -> comment.MULTI,     // /*
    ) ++
    (DFA.allAscii filterNot '*'.== ).map( c =>        // /* fooBAR! 123
      (comment.MULTI, c)        -> comment.MULTI ).toMap ++
    Map (
      (comment.MULTI, '*')      -> comment.STAR2,     // /* foo *
      (comment.STAR2, '*')      -> comment.STAR2,     // /* foo **
    ) ++
    (DFA.allAscii filterNot '/'.== ).map( c =>        // /* foo *** bar
      (comment.STAR2, c)        -> comment.MULTI ).toMap ++
    Map (
      (comment.STAR2, '/')      -> comment.FWD_SLASH3 // /* foo */
    )

  val strTransitions: Map[(Value, Char), Value] =
    // transitions for string literals
    Map (
      (START, '\"')             -> strLit.QUOTE,      // "
    ) ++
                                                      // "f
    (DFA.allAscii filterNot ((c: Char) => c == '\"' || c == '\\') ).map(c =>
      (strLit.QUOTE, c)         -> strLit.STR ).toMap ++
                                                      // "foo
    (DFA.allAscii filterNot ((c: Char) => c == '\"' || c == '\\') ).map(c =>
      (strLit.STR, c)           -> strLit.STR ).toMap ++
    Map (
      (strLit.QUOTE, '\\')      -> strLit.ESC,        // "\
      (strLit.STR, '\\')        -> strLit.ESC,        // "foo \
      (strLit.STR, '\"')        -> strLit.QUOTE2,     // "foo"
      (strLit.QUOTE, '\"')      -> strLit.QUOTE2      // ""
    ) ++
    DFA.whitespace.map( c =>                          // "foo"_
      (strLit.QUOTE2, c)        -> strLit.PRE_SPACE ).toMap ++
    DFA.whitespace.map( c =>                          // "foo"____
      (strLit.PRE_SPACE, c)     -> strLit.PRE_SPACE ).toMap ++
    Map (
      (strLit.PRE_SPACE, '+')   -> strLit.PLUS        // "foo"____+
    ) ++
    DFA.whitespace.map( c =>                          // "foo"____+_
      (strLit.PLUS, c)          -> strLit.POST_SPACE ).toMap ++
    DFA.whitespace.map( c =>                          // "foo"____+____
      (strLit.POST_SPACE, c)    -> strLit.POST_SPACE ).toMap ++
    Map (
      (strLit.POST_SPACE, '\"') -> strLit.STR         // "foo"____+____"
    )

//  val charTransitions: Map[(Value, Char), Value] = {
//    Map (
//      (START, '\'')             ->
//    )
//  }

  def escTransitions(from: Value, to: Value): Map[(Value, Char), Value] = {
    // transitions for escape characters
    DFA.escapeChars.map( c =>                       // "\n
      (from, c)                 -> to ).toMap ++
    // transitions for octal escapes in strings
    (DFA.digits filter '3'.>= ).map( c =>           // "\0
      (from, c)                 -> octal.DIGIT1).toMap ++
    (DFA.digits filter '4'.<= ).map( c =>           // "\8
      (from, c)                 -> octal.DIGIT2).toMap ++
    DFA.digits.map( c =>                            // "\06
      (octal.DIGIT1, c)         -> octal.DIGIT2).toMap ++
    DFA.digits.map( c =>                            // "\064
      (octal.DIGIT2, c)         -> to ).toMap
  }

  val transitions: Map[(Value, Char), Value] =
    commentTransitions ++
    strTransitions ++
    escTransitions(strLit.ESC, strLit.STR)

  def receive: PartialFunction[Any, Unit] = {
    case c: Char => sender() ! run(c)
    case EOF() => sender() ! Some(lastToken)
  }
}
