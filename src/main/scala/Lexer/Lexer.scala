package Lexer

import scala.io.{BufferedSource, Source}
import akka.pattern.ask
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class Status(val fileName: String) {
  val file: BufferedSource = Source.fromFile( fileName )
  val fileList: List[Char] = file.toList
  private var lexeme: String = ""
  private var row: Int = 1
  private var col: Int = 1
  private var charNum: Int = 0
  private var prev: Int = 0
  private def char: Char = { fileList.apply(charNum) }

  def getLexeme: String = { lexeme }
  def getRow: Int = { row }
  def getCol: Int = { col }
  def getChar: Char = { char }

  private[Lexer] def advance(): Unit = {
    charNum += 1
    col += 1
    if ( !eof ) {
      if ( char == '\n' ) {
        row += 1
        col = 0
      }
      lexeme += char
    }
  }

  private[Lexer] def restoreTo(token: Token.Token): Unit = {
    row = token.row
    col = token.col + 1
    charNum = prev + token.lexeme.toString.length
    prev = charNum
    lexeme = ""
  }

  private[Lexer] def eof: Boolean = {
    if ( charNum < fileList.size ) false
    else true
  }

  // returns false if eof is hit, true otherwise
  private[Lexer] def trimWhitespace(): Boolean = {
    while ( !eof && DFA.whitespace.contains( fileList.apply(charNum) )) {
      advance()
    }
    if ( !eof ) {
      prev = charNum
      lexeme = char.toString
      true
    } else {
      lexeme = ""
      false
    }
  }

  private[Lexer] def close(): Unit = {
    file.close
  }
}

class Lexer(actorSystem: ActorSystem) extends Actor {

  def tokenize ( fileName: String ): List[Token.Token] = {
    var tokens: ListBuffer[Token.Token] = ListBuffer()
    val status = new Status( fileName )
    val DFAs: Array[ActorRef] = Array(
      actorSystem.actorOf( Props(new LiteralDFA(status)), "LiteralDFA" ),
      actorSystem.actorOf( Props(new IdentifierDFA(status)), "IdentifierDFA" ),
      actorSystem.actorOf( Props(new PunctuationDFA(status)), "PunctuationDFA" )
    )

    while( status.trimWhitespace() ) {

      // Meta data containers
      implicit object Ord extends Ordering[(Int, Token.Token)] {
        def compare(x: (Int, Token.Token), y: (Int, Token.Token)): Int = x._1.compare(y._1)
      }
      var resultHeap: mutable.PriorityQueue[(Int, Token.Token)] = mutable.PriorityQueue.empty[(Int, Token.Token)]
      var activeDFAs: Set[ActorRef] = DFAs.toSet


      do {
        implicit val timeout: Timeout = 5 second

        // ask each dfa
        val futures: Map[ActorRef, Option[Future[Any]]] =
          if ( !status.eof )
            ( for( dfa <- activeDFAs ) yield dfa -> Some(dfa ask status.getChar) ).toMap
          else
            // we've hit eof, have each remaining dfa report its last accept state
            ( for( dfa <- activeDFAs ) yield dfa -> Some(dfa ask EOF()) ).toMap

        // read each response
        for( f <- futures ) {
          val token: Option[Option[Token.Token]]
            = Await.result( f._2.get, Duration.Inf ).asInstanceOf[Option[Option[Token.Token]]]
          token match {
            case None =>
              // the dfa can continue accepting input
              // don't do anything

            case Some(None) =>
              // the dfa never hit an accepting state
              // just remove it from activeDFAs
              activeDFAs = activeDFAs - f._1

            case Some(Some(t)) =>
              // the dfa hit an error state and could move no further
              // store the result and remove from activeDFAs
              val r = (t.lexeme.toString.length, t)
              resultHeap += r
              activeDFAs = activeDFAs - f._1
          }
        }

      } while(
        if ( activeDFAs.nonEmpty ) {
          if ( !status.eof ) status.advance()
          true
        }
        else
          false
      )

      // the DFAs have finished, take the longest result
      if ( resultHeap.nonEmpty ) {
        // top of the heap will be the longest match and thus the token we want
        val top: (Int, Token.Token) = resultHeap.max

        // add the token to the list
        tokens += top._2

        // reset charNum back to the next char to be processed
        status.restoreTo(top._2)
      } else {
        // no dfa returned a token, throw an error
        println("thing happened")
        throw Error.Error(status.getLexeme, "Unable to tokenize", Error.Type.Lexer,
          Some( Error.Location(status.getRow, status.getCol, status.fileName)))
      }
    }

    status.close()
    tokens.toList
  }

  override def receive: Receive = {
    case f: String => sender() ! tokenize(f)
  }
}
