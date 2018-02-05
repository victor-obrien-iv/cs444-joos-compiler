package Lexer

import Token.Token

import scala.StringContext.InvalidEscapeException
import scala.math


object LiteralDFA extends Enumeration {
  val START: Value = Value

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
    // states for an octal escape, ex: \064
    object octal {
      val DIGIT1, DIGIT2 = Value
    }
  }

  object int {
    val ZERO, INT = Value
    // intMax in JOOS is 2^31 -1, but intMin is -2^31
    // without the '-' 2^31 is invalid. Thus it has its own states
    object min {
      val NEG, SPACE, D2, D21, D214, D21474, D2147, D214748,
      D2147483, D21474836, D214748364, D2147483648 = Value
    }
  }
}

class LiteralDFA(status: Status) extends DFA[LiteralDFA.Value](status) {
  import LiteralDFA._

  val startState: Value = START
  var currentState: Value = startState

  var text = ""
  def textState: Boolean = {
    currentState == str.STR || currentState == char.CHAR ||
    currentState == str.ESC || currentState == char.ESC ||
    currentState == char.octal.DIGIT1 || currentState == char.octal.DIGIT2
  }

  val acceptingStates: Map[Value, ()  => Token] = Map(
    comment.SINGLE      -> ( () => Token.Comment.apply (status.getLexeme, status.getRow, status.getCol) ),
    comment.NEWLINE     -> ( () => Token.Comment.apply (status.getLexeme, status.getRow, status.getCol) ),
    comment.FWD_SLASH3  -> ( () => Token.Comment.apply (status.getLexeme, status.getRow, status.getCol) ),
    str.QUOTE2          ->
      (() => {
        try {
          val unescaped: String = StringContext.treatEscapes(text)
          Token.StringLiteral.apply(status.getLexeme, status.getRow, status.getCol, unescaped)
        }
        catch {
          case _: InvalidEscapeException =>
            // the treadEscapes method failed due to a bad escape char
            status.reporter ! Error.Error(status.getLexeme,
              "bad escape character", Error.Type.LiteralDFA, Some( Error.Location(status.getRow, status.getCol, status.fileName)))
            // this still needs to return a token, just return a str token with the untreated text
            Token.StringLiteral.apply(status.getLexeme, status.getRow, status.getCol, text)
        }
      }),
    char.APOST2         ->
      (() => {
        val unescaped: String = StringContext.treatEscapes(text)
        assert(unescaped.length == 1, "char must be of size 1")
        Token.CharacterLiteral.apply (status.getLexeme, status.getRow, status.getCol, unescaped.charAt(0))
      }),
    int.ZERO            -> ( () => Token.IntegerLiteral.apply (status.getLexeme, status.getRow, status.getCol, 0)),
    int.INT             ->
      (() => {
        def tooBig(): Unit = {
          status.reporter ! Error.Error(status.getLexeme,
            "integer literal exceeds integer maximum of " + (Int.MaxValue).toString,
            Error.Type.LiteralDFA, Some( Error.Location(status.getRow, status.getCol, status.fileName)))
        }

        if ( status.getLexeme.length > 10 ) tooBig()

        val number: BigInt = BigInt(status.getLexeme, 10)
          if ( number > Int.MaxValue ) tooBig()
          Token.IntegerLiteral.apply (status.getLexeme, status.getRow, status.getCol, number.toInt)
      }),
    int.min.D2147483648 -> (() => Token.IntegerLiteral.apply (status.getLexeme, status.getRow, status.getCol, Int.MinValue) )
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
      (str.PRE_SPACE, '+')      -> str.PLUS,          // "foo"____+
      (str.QUOTE2, '+')         -> str.PLUS           // "foo"+
    ) ++
    DFA.whitespace.map( c =>                          // "foo"____+_
      (str.PLUS, c)             -> str.POST_SPACE ).toMap ++
    DFA.whitespace.map( c =>                          // "foo"____+____
      (str.POST_SPACE, c)       -> str.POST_SPACE ).toMap ++
    Map (
      (str.POST_SPACE, '\"')    -> str.QUOTE,         // "foo"____+____"
      (str.PLUS, '\"')          -> str.QUOTE          // "foo"+"
    ) ++
    // transition for escape characters
    DFA.escapeChars.map( c =>                         // "\n
      (str.ESC, c)              -> str.STR ).toMap ++
    DFA.octDigits.map( c =>                           // "\0
      (str.ESC, c)              -> str.STR ).toMap

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
    ) ++
    // transition for escape characters
    DFA.escapeChars.map( c =>                         // '\n
      (char.ESC, c)             -> char.CHAR ).toMap  ++
    // transitions for octal escapes in strings
    (DFA.digits filter '3'.>= ).map( c =>             // '\0
      (char.ESC, c)             -> char.octal.DIGIT1).toMap ++
    (DFA.digits filter '4'.<= ).map( c =>             // '\8
      (char.ESC, c)             -> char.octal.DIGIT2).toMap ++
    DFA.digits.map( c =>                              // '\06
      (char.octal.DIGIT1, c)    -> char.octal.DIGIT2).toMap ++
    DFA.digits.map( c =>                              // '\064
      (char.octal.DIGIT2, c)    -> char.CHAR ).toMap ++
    Map (
      (char.octal.DIGIT1, '\'') -> char.APOST2,       // '\06'
      (char.octal.DIGIT2, '\'') -> char.APOST2        // '\064'
    )

  val intTransitions: Map[(Value, Char), Value] =
    Map (
      (START, '0')              -> int.ZERO,          // 0
    ) ++
    DFA.oneToNine.map( c =>                           // 3
      (START, c)                -> int.INT ).toMap ++
    DFA.digits.map( c =>                              // 36
      (int.INT, c)              -> int.INT ).toMap ++
    // transitions for intMin special case
    Map (
      (START, '-')              -> int.min.NEG        // -
    ) ++
    DFA.whitespace.map( c =>                          // -_
      (int.min.NEG, c)          -> int.min.SPACE ).toMap ++
    DFA.whitespace.map( c =>                          // -___
      (int.min.SPACE, c)        -> int.min.SPACE ).toMap ++
    Map (
      (int.min.SPACE, '2')      -> int.min.D2,
      (int.min.NEG, '2')        -> int.min.D2,
      (int.min.D2, '1')         -> int.min.D21,
      (int.min.D21, '4')        -> int.min.D214,
      (int.min.D214, '7')       -> int.min.D2147,
      (int.min.D2147, '4')      -> int.min.D21474,
      (int.min.D21474, '8')     -> int.min.D214748,
      (int.min.D214748, '3')    -> int.min.D2147483,
      (int.min.D2147483, '6')   -> int.min.D21474836,
      (int.min.D21474836, '4')  -> int.min.D214748364,
      (int.min.D214748364, '8') -> int.min.D2147483648
    )



  val transitions: Map[(Value, Char), Value] =
    commentTransitions ++
    strTransitions ++
    charTransitions ++
    intTransitions

  def receive: PartialFunction[Any, Unit] = {
    case c: Char =>
      sender() ! run(c)
      if ( textState ) text += c
    case EOF() =>
      sender() ! Some(lastToken)
  }

  override def reset(): Unit = {
    super.reset()
    text = ""
  }
}
