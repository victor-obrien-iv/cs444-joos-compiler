package Token

/**
  * Token represents a token
  */
sealed trait Token {
  def lexeme: Any
  def row: Int
  def col: Int
}

/**
  * Identifier is unlimited-length sequence of Java letters and digits
  * @param lexeme The sequence of Java letters and digits
  * @param row The row in the source that the token appeared
  * @param col The column in the source that token appeared
  */
case class Identifier(lexeme: String, row: Int, col: Int) extends Token


/**
  * Comment represents a comment in the Java source code.
  */
case class Comment(lexeme: String, row: Int, col: Int) extends Token

/**
  * Keyword represents a Java keyword. Identifiers should be checked so they don't conflict
  * with these keywords. Java has been prefixed so not to clash with Scala types
  */
sealed trait Keyword extends Token 

case class JavaAbstract(lexeme:String = "abstract", row: Int, col: Int) extends Keyword
case class JavaBoolean(lexeme:String = "boolean", row: Int, col: Int) extends Keyword
case class JavaBreak(lexeme:String = "break", row: Int, col: Int) extends Keyword
case class JavaByte(lexeme:String = "byte", row: Int, col: Int) extends Keyword
case class JavaCase(lexeme:String ="case", row: Int, col: Int) extends Keyword
case class JavaCatch(lexeme:String = "catch", row: Int, col: Int) extends Keyword
case class JavaChar(lexeme:String = "char", row: Int, col: Int) extends Keyword
case class JavaClass(lexeme:String = "class", row: Int, col: Int) extends Keyword
case class JavaConst(lexeme:String = "const", row: Int, col: Int) extends Keyword
case class JavaContinue(lexeme:String = "continue", row: Int, col: Int) extends Keyword
case class JavaDefault(lexeme:String = "default", row: Int, col: Int) extends Keyword
case class JavaDo(lexeme:String = "do", row: Int, col: Int) extends Keyword
case class JavaDouble(lexeme:String = "double", row: Int, col: Int) extends Keyword
case class JavaElse(lexeme:String = "else", row: Int, col: Int) extends Keyword
case class JavaExtends(lexeme:String = "extends", row: Int, col: Int) extends Keyword
case class JavaFinal(lexeme:String = "final", row: Int, col: Int) extends Keyword
case class JavaFinally(lexeme:String = "finally", row: Int, col: Int) extends Keyword
case class JavaFloat(lexeme:String = "float", row: Int, col: Int) extends Keyword
case class JavaFor(lexeme:String = "for", row: Int, col: Int) extends Keyword
case class JavaGoto(lexeme:String = "goto", row: Int, col: Int) extends Keyword
case class JavaIf(lexeme:String = "if", row: Int, col: Int) extends Keyword
case class JavaImplements(lexeme:String = "implements", row: Int, col: Int) extends Keyword
case class JavaImport(lexeme:String = "import", row: Int, col: Int) extends Keyword
case class JavaInt(lexeme:String = "int", row: Int, col: Int) extends Keyword
case class JavaInterface(lexeme:String = "interface", row: Int, col: Int) extends Keyword
case class JavaLong(lexeme:String = "long", row: Int, col: Int) extends Keyword
case class JavaNative(lexeme:String = "native", row: Int, col: Int) extends Keyword
case class JavaNew(lexeme:String = "new", row: Int, col: Int) extends Keyword
case class JavaPackage(lexeme:String = "package", row: Int, col: Int) extends Keyword
case class JavaPrivate(lexeme:String = "private", row: Int, col: Int) extends Keyword
case class JavaProtected(lexeme:String = "protected", row: Int, col: Int) extends Keyword
case class JavaPublic(lexeme:String = "public", row: Int, col: Int) extends Keyword
case class JavaReturn(lexeme:String = "return", row: Int, col: Int) extends Keyword
case class JavaShort(lexeme:String = "short", row: Int, col: Int) extends Keyword
case class JavaStatic(lexeme:String = "static", row: Int, col: Int) extends Keyword
case class JavaStrictfp(lexeme:String = "strictfp", row: Int, col: Int) extends Keyword
case class JavaSuper(lexeme:String = "super", row: Int, col: Int) extends Keyword
case class JavaSwitch(lexeme:String = "switch", row: Int, col: Int) extends Keyword
case class JavaSyncronized(lexeme:String = "syncronized", row: Int, col: Int) extends Keyword
case class JavaThis(lexeme:String = "this", row: Int, col: Int) extends Keyword
case class JavaThrow(lexeme:String = "throw", row: Int, col: Int) extends Keyword
case class JavaThrows(lexeme:String = "throws", row: Int, col: Int) extends Keyword
case class JavaTransient(lexeme:String = "transient", row: Int, col: Int) extends Keyword
case class JavaTry(lexeme:String = "try", row: Int, col: Int) extends Keyword
case class JavaVoid(lexeme:String = "void", row: Int, col: Int) extends Keyword
case class JavaVolatile(lexeme:String = "volatile", row: Int, col: Int) extends Keyword
case class JavaWhile(lexeme:String = "while", row: Int, col: Int) extends Keyword

/**
  * Literal represents a Java literal. Identifiers should be checked against null and booleans
  */
sealed trait Literal extends Token

case class IntegerLiteral(lexeme: String, row: Int, col: Int, value: Int) extends Literal
case class BooleanLiteral(lexeme: Boolean, row: Int, col: Int) extends Literal
case class CharacterLiteral(lexeme: String, row: Int, col: Int, value: Char) extends Literal
case class StringLiteral(lexeme: String, row: Int, col: Int, value: String) extends Literal
case class NullLiteral(lexeme: Any = null, row: Int, col: Int) extends Literal

/**
  * Java Separators
  */
sealed trait Separator extends Token

case class LParen(lexeme:String = "(", row: Int, col: Int) extends Separator
case class RParen(lexeme:String = ")", row: Int, col: Int) extends Separator
case class LBrace(lexeme:String = "{", row: Int, col: Int) extends Separator
case class RBrace(lexeme:String = "}", row: Int, col: Int) extends Separator
case class LBrack(lexeme:String = "[", row: Int, col: Int) extends Separator
case class RBrack(lexeme:String = "]", row: Int, col: Int) extends Separator
case class SemiColon(lexeme:String = ":",  row: Int, col: Int) extends Separator
case class Comma(lexeme:String = ",", row: Int, col: Int) extends Separator
case class Dot(lexeme:String = ".", row: Int, col: Int) extends Separator

/**
  * Java Operators
  */
sealed trait Operator extends Token

case class Becomes(lexeme:String = "=", row: Int, col: Int) extends Operator
case class GT(lexeme:String = ">", row: Int, col: Int) extends Operator
case class LT(lexeme:String = "<", row: Int, col: Int) extends Operator
case class EQ(lexeme:String = "==", row: Int, col: Int) extends Operator
case class GE(lexeme:String = ">=", row: Int, col: Int) extends Operator
case class LE(lexeme:String = "<=", row: Int, col: Int) extends Operator
case class NE(lexeme:String = "!=", row: Int, col: Int) extends Operator
case class Bang(lexeme:String = "!", row: Int, col: Int) extends Operator
case class AmbAmb(lexeme:String = "&&", row: Int, col: Int) extends Operator
case class LineLine(lexeme:String = "||", row: Int, col: Int) extends Operator
case class Amb(lexeme:String = "&", row: Int, col: Int) extends Operator
case class Line(lexeme:String = "|", row: Int, col: Int) extends Operator
case class Plus(lexeme:String = "+", row: Int, col: Int) extends Operator
case class Minus(lexeme:String ="-", row: Int, col: Int) extends Operator
case class Star(lexeme:String = "*", row: Int, col: Int) extends Operator
case class Slash(lexeme:String = "/", row: Int, col: Int) extends Operator
case class Percent(lexeme:String = "%", row: Int, col: Int) extends Operator