package Lexer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}
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
    if ( char == '\n' ) {
      row += 1
      col = 0
    }
    else col += 1
    charNum += 1
    if ( !eof ) lexeme += char
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

  private[Lexer] def nextLine(): Unit = {
    while ( !eof && char != '\n' ) advance()
  }

  private[Lexer] def close(): Unit = {
    file.close
  }
}

class Lexer() {

  def tokenize ( fileName: String ): List[Token.Token] = {
    var tokens: ListBuffer[Token.Token] = ListBuffer()
    val status = new Status(fileName)
    val DFAs: List[DFA[_]] = List(
      new LiteralDFA(status),
      new IdentifierDFA(status),
      new PunctuationDFA(status)
    )

    while( status.trimWhitespace() ) {

      // Meta data containers
      implicit object Ord extends Ordering[(Int, Token.Token)] {
        def compare(x: (Int, Token.Token), y: (Int, Token.Token)): Int = x._1.compare(y._1)
      }
      var resultHeap: mutable.PriorityQueue[(Int, Token.Token)] = mutable.PriorityQueue.empty[(Int, Token.Token)]

      var activeDFAs: List[DFA[_]] = DFAs
      while ( activeDFAs.nonEmpty ) {
        def runDFAs(dfas: List[DFA[_]]): List[DFA[_]] = {
          var next: List[DFA[_]] = List()
          for( dfa <- dfas ) {
            val token = if (!status.eof) dfa.run(status.getChar) else dfa.getLastToken
            token match {
              case None =>
                // the dfa can continue accepting input
                // append it to the active list
                next = next.::(dfa)

              case Some(None) =>
              // the dfa never hit an accepting state
              // just remove it from activeDFAs

              case Some(Some(t)) =>
                // the dfa hit an error state and could move no further
                // store the result and remove from activeDFAs
                val r = (t.lexeme.toString.length, t)
                resultHeap += r
            }
          }
          next
        }
        activeDFAs = runDFAs(activeDFAs)

        if (activeDFAs.nonEmpty && !status.eof)
          status.advance()
      }


      // the DFAs have finished, take the longest result
      if ( resultHeap.nonEmpty ) {
        // top of the heap will be the longest match and thus the token we want
        val top: (Int, Token.Token) = resultHeap.max

        // add the token to the list
        tokens += top._2

        // reset charNum back to the next char to be processed
        status.restoreTo(top._2)
      } else {
        // no dfa returned a token, give the reporter an error
        throw Error.Error(status.getLexeme, "Unable to tokenize", Error.Type.Lexer,
          Some( Error.Location(status.getRow, status.getCol, status.fileName)))
      }
    }

    status.close()
    tokens.toList
  }

}
