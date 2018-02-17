package Error

import scala.io.{BufferedSource, Source}
import scala.collection.mutable.ListBuffer

import akka.actor.Actor

case class Report()

class Reporter extends Actor {
  private var errors: ListBuffer[Error] = ListBuffer()

  private def print(err: Error): Unit = {
    val str = err.kind match {
      case Type.CommandLine   => "An error occurred with command line argument: " + err.cause
      case Type.Lexer         => "Failure to tokenize: " + err.cause
      case Type.LiteralDFA    => "Literal could not be tokenized, " + err.cause
      case _                  => "An error occurred: " + err.cause
    }
    println( str )
    err.loc match {
      case Some(l) =>
        val file: BufferedSource = Source.fromFile( l.file )
        val lines: Array[String] = file.getLines().toArray
        val errorLine = if (l.lineNum > 0) l.lineNum - 1 else 0
        val errorCol = if (l.col > 0) l.col - 1 else 0

        println(lines(errorLine))
        println("~" * (errorCol) + "^")
        println("\t" + l.file + " at " + l.lineNum + ":" + l.col)
      case None =>
    }
    println( "\t" + err.msg )
  }

  override def receive: Receive = {
    case e: Error =>
      errors += e
    case Report =>
      for ( e <- errors ) print(e) // TODO: have this print in file order
      sender() ! errors.nonEmpty
  }
}
