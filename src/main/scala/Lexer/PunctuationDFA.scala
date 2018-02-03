package Lexer

import Token.Token

object PunctuationDFA extends Enumeration {
  val START = Value

  object separator {
    val LPAREN, RPAREN, LBRACE, RBRACE, LBRACK,
      RBRACK, SEMICOLON, COMMA, DOT = Value
  }

  object operator {
    val BECOMES, GT, LT, EQ, GE, LE, NE,
      BANG, AMPAMP, BARBAR, AMP, BAR, PLUS,
      MINUS, STAR, SLASH, PERCENT = Value
  }
}

class PunctuationDFA(status: Lexer.Status) extends DFA[PunctuationDFA.Value](status) {
  import PunctuationDFA._

  val startState: Value = START
  var currentState: Value = startState

  val acceptingStates: Map[Value, ()  => Token] = Map(
    // separators
    separator.LPAREN      -> ( () => Token.LParen.apply (row = status.getRow, col = status.getCol) ),
    separator.RPAREN      -> ( () => Token.RParen.apply (row = status.getRow, col = status.getCol) ),
    separator.LBRACE      -> ( () => Token.LBrace.apply (row = status.getRow, col = status.getCol) ),
    separator.RBRACE      -> ( () => Token.RBrace.apply (row = status.getRow, col = status.getCol) ),
    separator.LBRACK      -> ( () => Token.LBrack.apply (row = status.getRow, col = status.getCol) ),
    separator.RBRACK      -> ( () => Token.RBrack.apply (row = status.getRow, col = status.getCol) ),
    separator.SEMICOLON   -> ( () => Token.SemiColon.apply (row = status.getRow, col = status.getCol) ),
    separator.COMMA       -> ( () => Token.Comma.apply (row = status.getRow, col = status.getCol) ),
    separator.DOT         -> ( () => Token.Dot.apply (row = status.getRow, col = status.getCol) ),
    // operators
    operator.BECOMES      -> ( () => Token.Becomes.apply (row = status.getRow, col = status.getCol) ),
    operator.GT           -> ( () => Token.GT.apply (row = status.getRow, col = status.getCol) ),
    operator.LT           -> ( () => Token.LT.apply (row = status.getRow, col = status.getCol) ),
    operator.EQ           -> ( () => Token.EQ.apply (row = status.getRow, col = status.getCol) ),
    operator.GE           -> ( () => Token.GE.apply (row = status.getRow, col = status.getCol) ),
    operator.LE           -> ( () => Token.LE.apply (row = status.getRow, col = status.getCol) ),
    operator.NE           -> ( () => Token.NE.apply (row = status.getRow, col = status.getCol) ),
    operator.BANG         -> ( () => Token.Bang.apply (row = status.getRow, col = status.getCol) ),
    operator.AMPAMP       -> ( () => Token.AmpAmp.apply (row = status.getRow, col = status.getCol) ),
    operator.BARBAR       -> ( () => Token.BarBar.apply (row = status.getRow, col = status.getCol) ),
    operator.AMP          -> ( () => Token.Amp.apply (row = status.getRow, col = status.getCol) ),
    operator.BAR          -> ( () => Token.Bar.apply (row = status.getRow, col = status.getCol) ),
    operator.PLUS         -> ( () => Token.Plus.apply (row = status.getRow, col = status.getCol) ),
    operator.MINUS        -> ( () => Token.Minus.apply (row = status.getRow, col = status.getCol) ),
    operator.STAR         -> ( () => Token.Star.apply (row = status.getRow, col = status.getCol) ),
    operator.SLASH        -> ( () => Token.Slash.apply (row = status.getRow, col = status.getCol) ),
    operator.PERCENT      -> ( () => Token.Percent.apply (row = status.getRow, col = status.getCol) ),
  )

  val separatorTransitions: Map[(Value, Char), Value] = Map(
    (START, '(')          -> separator.LPAREN,
    (START, ')')          -> separator.RPAREN,
    (START, '{')          -> separator.LBRACE,
    (START, '}')          -> separator.RBRACE,
    (START, '[')          -> separator.LBRACK,
    (START, ']')          -> separator.RBRACK,
    (START, ';')          -> separator.SEMICOLON,
    (START, ',')          -> separator.COMMA,
    (START, '.')          -> separator.DOT
  )

  val operatorTransitions: Map[(Value, Char), Value] = Map(
    (START, '=')              -> operator.BECOMES,
    (START, '>')              -> operator.GT,
    (START, '<')              -> operator.LT,
    (operator.BECOMES, '=')   -> operator.EQ,       // ==
    (operator.GT, '=')        -> operator.GE,       // >=
    (operator.LT, '=')        -> operator.LE,       // <=
    (operator.BANG, '=')      -> operator.NE,       // !=
    (START, '!')              -> operator.BANG,
    (operator.AMP, '&')       -> operator.AMPAMP,   // &&
    (operator.BAR, '&')       -> operator.BARBAR,   // ||
    (START, '&')              -> operator.AMP,
    (START, '|')              -> operator.BAR,
    (START, '+')              -> operator.PLUS,
    (START, '-')              -> operator.MINUS,
    (START, '/')              -> operator.SLASH,
    (START, '%')              -> operator.PERCENT,
  )

  val transitions: Map[(Value, Char), Value] =
    separatorTransitions ++ operatorTransitions

  def receive: PartialFunction[Any, Unit] = {
    case c: Char =>
      sender() ! run(c)
    case EOF() =>
      sender() ! Some(lastToken)
  }
}
