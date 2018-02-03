package Driver

import akka.pattern.ask
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

object Driver {
  def main(args: Array[String]): Unit = {
    val actorSystem: ActorSystem = ActorSystem( "actorSystem" )
    implicit val timeout: Timeout = 5 second

    try {
      CommandLine parseArgs args
      if ( CommandLine.files.isEmpty ) throw Error.Error( "", "No .java file specified", Error.Type.CommandLine )

      val lexer = actorSystem.actorOf( Props(new Lexer.Lexer(actorSystem)), "Lexer" )
      val tokens: Seq[(String, Future[Any])] = for(f <- CommandLine.files ) yield (f, lexer ask f)
      // just print out the tokens for now
      for( ft <- tokens ) {
        println(ft._1 + ":")
        val tokenList = Await.result(ft._2, Duration.Inf).asInstanceOf[List[Token.Token]]
        println(tokenList)
      }


    } catch {
      case err:
        Error.Error => Error.Reporter.print( err )
        println("exit: 42")
        System.exit(42) // the input file is not lexically/syntactically valid Joos 1W
    } finally {
      actorSystem.terminate()
    }
    println("exit: 0")
    System.exit(0) // the input file is lexically/syntactically valid Joos 1W

  }
}
