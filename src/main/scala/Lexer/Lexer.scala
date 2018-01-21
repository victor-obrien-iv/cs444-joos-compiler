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

  def tokenize ( file: BufferedSource ): List[Result] = {
    var tokens: ListBuffer[Result] = ListBuffer()
    val fileList = file.toList

    var i: Int = 0
    while( i < fileList.size ) {

      // Meta data containers
      implicit object Ord extends Ordering[(Int, Result)] {
        def compare(x: (Int, Result), y: (Int, Result)) = y._1.compare(x._1)
      }
      var resultHeap: PriorityQueue[(Int, Result)] = PriorityQueue.empty[(Int, Result)]
      val futures: Map[ActorRef, Option[Future[Any]]] = Map( DFAs.apply( 0 ) -> None )
      var activeDFAs: Set[ActorRef] = DFAs toSet
      val prev: Int = i

      //
      while ( activeDFAs.nonEmpty ) {

        implicit val timeout: Timeout = 5 second;
        for( dfa <- activeDFAs ) {
          if ( i < fileList.size ) {
            val c: Char = fileList.apply(i)
            lexeme += c
            futures(dfa) = Some(dfa ask c)
          }
          else
            // we've hit eof, have each remaining dfa report its last accept state
            futures( dfa ) = Some( dfa ask EOF() )
        }

        for( f <- futures ) {
          val r: Option[Result] = Await.result( f._2.get, Duration.Inf ).asInstanceOf[Option[Result]]
          if ( r.isDefined ) {
            // the dfa hit an error state and could move no further
            // store the result and remove from activeDFAs
            val ir = ( r.get.lexeme.length, r.get )
            resultHeap += ir
            activeDFAs = activeDFAs - f._1
          }
        }

        i += 1
      }


      // top of the heap will be the longest match and thus the token we want
      val r: (Int, Result) = resultHeap.max
      // reset i back to the next char to be processed
      i = prev + r._1
      lexeme = ""
      // add the token to the list
      // TODO: handle if the token is an error
      if ( r._2.token == "ERROR")
        i += 1
      tokens += r._2
    }

    actorSystem.terminate()

    tokens.toList
  }

}
