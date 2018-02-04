package Driver

import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

object Driver {
  val actorSystem: ActorSystem = ActorSystem( "actorSystem" )
  val reporter: ActorRef = actorSystem.actorOf( Props(new Error.Reporter), "Reporter" )
  implicit val timeout: Timeout = 5 second

  def ErrorExit(): Unit = {
    actorSystem.terminate()
    println("exit: 42")
    System.exit(42) // the input file is not lexically/syntactically valid Joos 1W
  }

  def CleanExit(): Unit = {
    actorSystem.terminate()
    println("exit: 0")
    System.exit(0) // the input file is lexically/syntactically valid Joos 1W
  }

  def errorsFound: Boolean = {
    val report = reporter ask Error.Report
    Await.result(report, Duration.Inf).asInstanceOf[Boolean]
  }

  def main(args: Array[String]): Unit = {
    // parse commandline args
    val commandLine = new CommandLine( args, reporter )
    if ( errorsFound ) ErrorExit()

    // create lexer actor
    val lexer = actorSystem.actorOf( Props(new Lexer.Lexer(actorSystem, reporter)), "Lexer" )
    val tokens: Seq[(String, Future[Any])] = for(f <- commandLine.files ) yield (f, lexer ask f)

    for( ft <- tokens ) {
      // just print out the tokens for now
      println(ft._1 + ":")
      val tokenList = Await.result(ft._2, Duration.Inf).asInstanceOf[List[Token.Token]]
      println(tokenList)
    }
    if ( errorsFound ) ErrorExit()

    CleanExit()
  }
}
