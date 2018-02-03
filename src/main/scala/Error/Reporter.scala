package Error

import scala.io.{BufferedSource, Source}

object Reporter {
  def print(err: Error): Unit = {
    val str = err.kind match {
      case Type.CommandLine => "An error occurred with command line argument: " + err.cause
      case _ => "An error occurred: " + err.cause
    }
    println( str )
    err.loc match {
      case Some(l) =>
        val file: BufferedSource = Source.fromFile( l.file )
        val lines: Array[String] = file.getLines().toArray
        println(lines(l.lineNum - 1))
        println("~" * (l.col - 1) + "^")
      case None =>
    }
    println( err.msg )
  }
}
