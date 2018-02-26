package Error

import scala.io.{BufferedSource, Source}

class ErrorFormatter {
   def format(err: Error): String = {
    val str = err.kind match {
      case Type.CommandLine   => "An error occurred with command line argument: " + err.cause
      case Type.Lexer         => "Failure to tokenize: " + err.cause
      case Type.LiteralDFA    => "Literal could not be tokenized, " + err.cause
      case _                  => "An error occurred: " + err.cause
    }

     val errorPointer = err.loc match {
      case Some(l) =>
        val file: BufferedSource = Source.fromFile( l.file )
        val lines: Array[String] = file.getLines().toArray
        val errorLine = if (l.lineNum > 0) l.lineNum - 1 else 0
        val errorCol = if (l.col > 0) l.col - 1 else 0

        s"${lines(errorLine)}\n${"~" * errorCol}^\n\t${l.file} at ${l.lineNum}:${l.col}"
      case None => ""
    }

    s"$str\n$errorPointer\n\t${err.msg}"
  }
}
