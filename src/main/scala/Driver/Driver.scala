package Driver

import scala.io.{BufferedSource, Source}

object Driver {
  def main(args: Array[String]): Unit = {
    try {
      CommandLine parseArgs args

      if ( CommandLine.files.isEmpty ) throw Error.Error( "", "No .java file specified", Error.Type.CommandLine )

      // TODO: do we even need worry about multiple files?
      for( f <- CommandLine.files ) {
        val file: BufferedSource = Source.fromFile( f )

        val tokens = Lexer.Lexer.tokenize( file )

        for( t <- tokens ) println( t ) // just print out the tokens for now
        file.close


      }


    } catch {
      case err: Error.Error => Error.Reporter.print( err )
    }

  }
}
