package Lexer

import Token.Token

/*
Keyword: one of
  abstract  default   if          private     this
  boolean   do        implements  protected   throw
  break     double    import      public      throws
  byte      else      instanceof  return      transient
  case      extends   int         short       try
  catch     final     interface   static      void
  char      finally   long        strictfp    volatile
  class     float     native      super       while
  const     for       new         switch
  continue  goto      package     synchronized

  Keywords are matched after a string has been identified as an id
*/

object IdentifierDFA extends Enumeration  {
  val START, ALPHA, ALPHANUM = Value
}

class IdentifierDFA(status: Status) extends DFA[IdentifierDFA.Value](status){
  import IdentifierDFA._

  val startState: Value = START
  var currentState: Value = startState

  val keywords: Map[String, () => Token] = Map(
    "abstract"      -> (() => Token.JavaAbstract(row = status.getRow, col = status.getCol)),
    "default"       -> (() => Token.JavaDefault(row = status.getRow, col = status.getCol)),
    "if"            -> (() => Token.JavaIf(row = status.getRow, col = status.getCol)),
    "private"       -> (() => Token.JavaPrivate(row = status.getRow, col = status.getCol)),
    "this"          -> (() => Token.JavaThis(row = status.getRow, col = status.getCol)),
    "boolean"       -> (() => Token.JavaBoolean(row = status.getRow, col = status.getCol)),
    "do"            -> (() => Token.JavaDo(row = status.getRow, col = status.getCol)),
    "implements"    -> (() => Token.JavaImplements(row = status.getRow, col = status.getCol)),
    "protected"     -> (() => Token.JavaProtected(row = status.getRow, col = status.getCol)),
    "throw"         -> (() => Token.JavaThrow(row = status.getRow, col = status.getCol)),
    "break"         -> (() => Token.JavaBreak(row = status.getRow, col = status.getCol)),
    "double"        -> (() => Token.JavaDouble(row = status.getRow, col = status.getCol)),
    "import"        -> (() => Token.JavaImport(row = status.getRow, col = status.getCol)),
    "public"        -> (() => Token.JavaPublic(row = status.getRow, col = status.getCol)),
    "throws"        -> (() => Token.JavaThrows(row = status.getRow, col = status.getCol)),
    "byte"          -> (() => Token.JavaByte(row = status.getRow, col = status.getCol)),
    "else"          -> (() => Token.JavaElse(row = status.getRow, col = status.getCol)),
    "instanceof"    -> (() => Token.JavaInstanceof(row = status.getRow, col = status.getCol)),
    "return"        -> (() => Token.JavaReturn(row = status.getRow, col = status.getCol)),
    "transient"     -> (() => Token.JavaTransient(row = status.getRow, col = status.getCol)),
    "case"          -> (() => Token.JavaCase(row = status.getRow, col = status.getCol)),
    "extends"       -> (() => Token.JavaExtends(row = status.getRow, col = status.getCol)),
    "int"           -> (() => Token.JavaInt(row = status.getRow, col = status.getCol)),
    "short"         -> (() => Token.JavaShort(row = status.getRow, col = status.getCol)),
    "try"           -> (() => Token.JavaTry(row = status.getRow, col = status.getCol)),
    "catch"         -> (() => Token.JavaCatch(row = status.getRow, col = status.getCol)),
    "final"         -> (() => Token.JavaFinal(row = status.getRow, col = status.getCol)),
    "interface"     -> (() => Token.JavaInterface(row = status.getRow, col = status.getCol)),
    "static"        -> (() => Token.JavaStatic(row = status.getRow, col = status.getCol)),
    "void"          -> (() => Token.JavaVoid(row = status.getRow, col = status.getCol)),
    "char"          -> (() => Token.JavaChar(row = status.getRow, col = status.getCol)),
    "finally"       -> (() => Token.JavaFinally(row = status.getRow, col = status.getCol)),
    "long"          -> (() => Token.JavaLong(row = status.getRow, col = status.getCol)),
    "strictfp"      -> (() => Token.JavaStrictfp(row = status.getRow, col = status.getCol)),
    "volatile"      -> (() => Token.JavaVolatile(row = status.getRow, col = status.getCol)),
    "class"         -> (() => Token.JavaClass(row = status.getRow, col = status.getCol)),
    "float"         -> (() => Token.JavaFloat(row = status.getRow, col = status.getCol)),
    "native"        -> (() => Token.JavaNative(row = status.getRow, col = status.getCol)),
    "super"         -> (() => Token.JavaSuper(row = status.getRow, col = status.getCol)),
    "while"         -> (() => Token.JavaWhile(row = status.getRow, col = status.getCol)),
    "const"         -> (() => Token.JavaConst(row = status.getRow, col = status.getCol)),
    "for"           -> (() => Token.JavaFor(row = status.getRow, col = status.getCol)),
    "new"           -> (() => Token.JavaNew(row = status.getRow, col = status.getCol)),
    "switch"        -> (() => Token.JavaSwitch(row = status.getRow, col = status.getCol)),
    "continue"      -> (() => Token.JavaContinue(row = status.getRow, col = status.getCol)),
    "goto"          -> (() => Token.JavaGoto(row = status.getRow, col = status.getCol)),
    "package"       -> (() => Token.JavaPackage(row = status.getRow, col = status.getCol)),
    "synchronized"  -> (() => Token.JavaSyncronized(row = status.getRow, col = status.getCol))
  )
  val wordLiterals: Map[String, () => Token] = Map(
    "true"          -> (() => Token.BooleanLiteral(status.getRow, status.getCol, true)),
    "false"         -> (() => Token.BooleanLiteral(status.getRow, status.getCol, false)),
    "null"          -> (() => Token.NullLiteral(row = status.getRow, col = status.getCol))
  )
  val reservedWords: Map[String, () => Token] = keywords ++ wordLiterals

  val acceptingStates: Map[Value, ()  => Token] = Map(
    ALPHA           -> (() => Token.Identifier(status.getLexeme, status.getRow, status.getCol)),
    ALPHANUM        -> (() => Token.Identifier(status.getLexeme, status.getRow, status.getCol)),
  )

  val transitions: Map[(Value, Char), Value] =
    DFA.javaLetters.map( c =>
      (START, c)    -> ALPHA
    ).toMap ++
    (DFA.javaLetters + DFA.digits).map( c =>
      (ALPHA, c)    -> ALPHANUM
    ).toMap ++
    (DFA.javaLetters + DFA.digits).map( c =>
      (ALPHANUM, c) -> ALPHANUM
    ).toMap

  private def getToken(token: Option[Option[Token.Token]]): Option[Option[Token.Token]] = {
    token match {
      case Some(Some(t)) if reservedWords.contains(t.lexeme) =>
        val keyToken: () => Token = reservedWords(t.lexeme)
        Some(Some(keyToken()))
      case _ =>
        token
    }
  }
  def receive: PartialFunction[Any, Unit] = {
    case c: Char =>
      sender() ! getToken(run(c))
    case EOF() =>
      sender() ! getToken(Some(lastToken))
  }
}
