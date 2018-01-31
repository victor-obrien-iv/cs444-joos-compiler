package Lexer

object LiteralDFA extends Enumeration {
  val START = Value

  // states used for comments
  object comment {
    val FWD_SLASH, FWD_SLASH2, SINGLE, NEWLINE,
        STAR, STAR2, FWD_SLASH3, MULTI = Value
  }

  // states used for string literals
  object str {
    val QUOTE, STR, ESC, QUOTE2, PRE_SPACE,
        PLUS, POST_SPACE = Value
  }

  // states for a char literal
  object char {
    val APOST, CHAR, ESC, APOST2 = Value
  }

  // states for an octal escape, ex: \064
  object octalEsc {
    val DIGIT1, DIGIT2 = Value
  }

//  object decInt {
//
//  }
//
//  object hexInt {
//
//  }
//
//  object octInt {
//
//  }
}

class LiteralDFA extends DFA[LiteralDFA.Value] {
  import LiteralDFA._

  val startState = START
  var currentState = startState

  val acceptingStates = Map(
    comment.SINGLE      -> Token.Comment.apply _,
    comment.NEWLINE     -> Token.Comment.apply _,
    comment.FWD_SLASH3  -> Token.Comment.apply _,
    str.QUOTE2          -> Token.StringLiteral.apply _
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
      (START, '\"')             -> str.QUOTE,         // "
    ) ++
                                                      // "f
    (DFA.allAscii filterNot ((c: Char) => c == '\"' || c == '\\') ).map(c =>
      (str.QUOTE, c)            -> str.STR ).toMap ++
                                                      // "foo
    (DFA.allAscii filterNot ((c: Char) => c == '\"' || c == '\\') ).map(c =>
      (str.STR, c)              -> str.STR ).toMap ++
    Map (
      (str.QUOTE, '\\')         -> str.ESC,           // "\
      (str.STR, '\\')           -> str.ESC,           // "foo \
      (str.STR, '\"')           -> str.QUOTE2,        // "foo \064"
      (str.QUOTE, '\"')         -> str.QUOTE2         // ""
    ) ++
    DFA.whitespace.map( c =>                          // "foo"_
      (str.QUOTE2, c)           -> str.PRE_SPACE ).toMap ++
    DFA.whitespace.map( c =>                          // "foo"____
      (str.PRE_SPACE, c)        -> str.PRE_SPACE ).toMap ++
    Map (
      (str.PRE_SPACE, '+')      -> str.PLUS           // "foo"____+
    ) ++
    DFA.whitespace.map( c =>                          // "foo"____+_
      (str.PLUS, c)             -> str.POST_SPACE ).toMap ++
    DFA.whitespace.map( c =>                          // "foo"____+____
      (str.POST_SPACE, c)       -> str.POST_SPACE ).toMap ++
    Map (
      (str.POST_SPACE, '\"')    -> str.STR            // "foo"____+____"
    )

  val charTransitions: Map[(Value, Char), Value] =
    Map (
      (START, '\'')             -> char.APOST,        // '
      (char.APOST, '\\')        -> char.ESC           // '\
    ) ++
                                                      // 'f
    (DFA.allAscii filterNot ((c: Char) => c == '\'' || c == '\\') ).map(c =>
      (char.APOST, c)           -> char.CHAR ).toMap ++
    Map (
      (char.CHAR, '\'')         -> char.APOST2        // 'f' '\064'
    )


  def escTransitions(from: Value, to: Value): Map[(Value, Char), Value] = {
    // transitions for escape characters
    DFA.escapeChars.map( c =>                         // "\n
      (from, c)                 -> to ).toMap ++
    // transitions for octal escapes in strings
    (DFA.digits filter '3'.>= ).map( c =>             // "\0
      (from, c)                 -> octalEsc.DIGIT1).toMap ++
    (DFA.digits filter '4'.<= ).map( c =>             // "\8
      (from, c)                 -> octalEsc.DIGIT2).toMap ++
    DFA.digits.map( c =>                              // "\06
      (octalEsc.DIGIT1, c)         -> octalEsc.DIGIT2).toMap ++
    DFA.digits.map( c =>                              // "\064
      (octalEsc.DIGIT2, c)         -> to ).toMap
  }

  val transitions: Map[(Value, Char), Value] =
    commentTransitions ++
    strTransitions ++
    escTransitions(str.ESC, str.STR) ++
    charTransitions ++
    escTransitions(char.ESC, char.CHAR)

  def receive: PartialFunction[Any, Unit] = {
    case c: Char => sender() ! run(c)
    case EOF() => sender() ! Some(lastToken)
  }
}
