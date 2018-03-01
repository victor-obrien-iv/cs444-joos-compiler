import Driver.{CommandLine, Driver}
import Error.ErrorFormatter
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem( "actorSystem" )
  implicit val timeout: Timeout = 5 seconds

  val reporter: ActorRef = actorSystem.actorOf( Props(new Error.Reporter), "Reporter" )
  val errorFormatter: ErrorFormatter = new ErrorFormatter

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

  val commandLine = new CommandLine(args, reporter)
  if (errorsFound) ErrorExit()

  val driver = new Driver(reporter)

  commandLine.files foreach {
    file => driver.compile(file) onComplete {
      case Success((ast, errors)) =>
        if (errors.exists(_.isFailure)) {
          errors.foreach {
            error =>
              error.recover {
                case e: Error.Error => println(errorFormatter.format(e))
                case _ => println("UNEXPECTED ERROR OCCURRED:"); println(error)
              }
          }
          ErrorExit()
        }
        else CleanExit()
      case Failure(e) =>
        e match {
          case error: Error.Error => println(errorFormatter.format(error))
          case _ => println("UNEXPECTED ERROR OCCURRED:"); println(e)
        }
        ErrorExit()
    }
  }
}
