package Lexer

import scala.io.BufferedSource
import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.collection.mutable.{Map, ListBuffer, PriorityQueue}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Lexer {
  val actorSystem = ActorSystem( "actorSystem" )
  val DFAs: Array[ActorRef] = Array(
    actorSystem.actorOf( Props[LiteralDFA], "LiteralDFA" )
  )

  var lexeme: String = ""
  var charNum, row, col: Int = 0 // TODO: add increments for row and col

  def tokenize ( file: BufferedSource ): List[Token.Token] = {
    var tokens: ListBuffer[Token.Token] = ListBuffer()
    val fileList = file.toList

    // TODO: consume leading whitespace

    while( charNum < fileList.size ) {

      // Meta data containers
      implicit object Ord extends Ordering[(Int, Token.Token)] {
        def compare(x: (Int, Token.Token), y: (Int, Token.Token)) = y._1.compare(x._1)
      }
      var resultHeap: PriorityQueue[(Int, Token.Token)] = PriorityQueue.empty[(Int, Token.Token)]
      val futures: Map[ActorRef, Option[Future[Any]]] = Map( DFAs.apply( 0 ) -> None )
      var activeDFAs: Set[ActorRef] = DFAs toSet
      val prev: Int = charNum

      while ( activeDFAs.nonEmpty ) {

        implicit val timeout: Timeout = 5 second;
        for( dfa <- activeDFAs ) {
          if ( charNum < fileList.size ) {
            val c: Char = fileList.apply(charNum)
            lexeme += c
            futures(dfa) = Some(dfa ask c)
          }
          else
            // we've hit eof, have each remaining dfa report its last accept state
            futures( dfa ) = Some( dfa ask EOF() )
        }

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
              val ir = (t.lexeme.toString.length, t)
              resultHeap += ir
              activeDFAs = activeDFAs - f._1
          }
        }

        charNum += 1
      }

      if ( resultHeap.size > 0 ) {
        // top of the heap will be the longest match and thus the token we want
        val r: (Int, Token.Token) = resultHeap.max

        // add the token to the list
        tokens += r._2

        // reset charNum back to the next char to be processed
        charNum = prev + r._1
        lexeme = ""
      } else {
        // no dfa returned a token, throw an error
        // TODO: throw the error
      }

    }

    actorSystem.terminate()

    tokens.toList
  }

}
