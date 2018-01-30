package Lexer

import scala.io.BufferedSource
import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.collection.mutable.{ListBuffer, Map, PriorityQueue}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

object Lexer {
  val actorSystem = ActorSystem( "actorSystem" )
  val DFAs: Array[ActorRef] = Array(
    actorSystem.actorOf( Props[LiteralDFA], "LiteralDFA" )
  )

  object state {
    private var lexeme: String = ""
    private var row, col: Int = 0
    private val eofChar = '\u0000'
    private var char: Char = eofChar
    def getLexeme: String = { lexeme }
    def getRow: Int = { row }
    def getCol: Int = { col }
    def getChar: Char = { char }

    private var charNum: Int = -1
    private var prev: Int = 0
    private[Lexer] def advance(file: List[Char]): Boolean = {
      charNum += 1
      col += 1
      if ( charNum >= file.size ) {
        // we've hit eof
        char = eofChar
        return false
      }

      char = file.apply(charNum)
      lexeme += char

      if ( char == '\n' ) {
        row += 1
        col = 0
      }

      true
    }
    private[Lexer] def restoreTo(token: Token.Token): Unit = {
      row = token.row
      col = token.col
      charNum = prev + token.lexeme.toString.length
      prev = charNum
      lexeme = ""
    }
    private[Lexer] def eof: Boolean = {
      if (char == eofChar) true
      else false
    }
    private[Lexer] def trimWhitespace(file: List[Char]): Unit = {
      while ( charNum < file.size
              && DFA.whitespace.contains( file.apply(charNum) )) {
        if ( file.apply(charNum) == '\n' ) {
          row += 1
          col = 0
        }
        charNum += 1
        col += 1
      }
      if ( charNum < file.size ) {
        char = file.apply(charNum)
        prev = charNum
        lexeme = char toString
      } else {
        char = eofChar
        lexeme = ""
      }
    }
  }


  def tokenize ( file: BufferedSource ): List[Token.Token] = {
    var tokens: ListBuffer[Token.Token] = ListBuffer()
    val fileList = file toList

    state.advance(fileList)

    // TODO: consume leading whitespace

    do {

      state.trimWhitespace(fileList)

      // Meta data containers
      implicit object Ord extends Ordering[(Int, Token.Token)] {
        def compare(x: (Int, Token.Token), y: (Int, Token.Token)) = y._1.compare(x._1)
      }
      var resultHeap: PriorityQueue[(Int, Token.Token)] = PriorityQueue.empty[(Int, Token.Token)]
      val futures: Map[ActorRef, Option[Future[Any]]] = Map( DFAs.apply( 0 ) -> None )
      var activeDFAs: Set[ActorRef] = DFAs toSet

//      state.checkpoint()

      while ( activeDFAs.nonEmpty ) {
        implicit val timeout: Timeout = 5 second

        for( dfa <- activeDFAs ) {
          if ( !state.eof ) {
//            val c: Char = fileList.apply(state.charNum)
//            state.lexeme += c
            futures(dfa) = Some(dfa ask state.getChar)
//            state.advance()
          }
          else
            // we've hit eof, have each remaining dfa report its last accept state
            futures( dfa ) = Some(dfa ask EOF())
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

//        state.charNum += 1
        state.advance(fileList)
      }

      if ( resultHeap.nonEmpty ) {
        // top of the heap will be the longest match and thus the token we want
        val top: (Int, Token.Token) = resultHeap.max

        // add the token to the list
        tokens += top._2

        // reset charNum back to the next char to be processed
//        state.charNum = prev + token._1
//        state.lexeme = ""
        state.restoreTo(top._2)
        state.advance(fileList)
      } else {
        // no dfa returned a token, throw an error
        // TODO: throw the error
        println("Error: " + state.getChar)
      }

    } while ( !state.eof )

    actorSystem.terminate()

    tokens.toList
  }

}
