package Error

case object Type extends Enumeration {
  val ASTBuilder, CommandLine, Lexer, LiteralDFA, Parser = Value
}

case class Location ( lineNum: Integer, col: Integer, file: String )

case class Error ( cause: String, msg: String, kind: Type.Value, loc: Option[Location] = None ) extends Exception
