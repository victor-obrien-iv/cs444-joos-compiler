package Error

case object Type extends Enumeration {
  val CommandLine, Lexer, LiteralDFA, Parser, Weeder, ModifiersPass, ASTBuilder, TypeLinking,
    EnvironmentPass, ExtendsPass, HierarchyCheck, MethodsPass, ReachabilityPass = Value
}

case class Location ( lineNum: Integer, col: Integer, file: String )

case class Error ( cause: String, msg: String, kind: Type.Value, loc: Option[Location] = None ) extends Exception
