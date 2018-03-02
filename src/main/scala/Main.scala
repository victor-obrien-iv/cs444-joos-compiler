import AST.{AstNode, CompilationUnit}
import Driver.{CommandLine, Driver}
import Error.ErrorFormatter
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem( "actorSystem" )
  implicit val timeout: Timeout = 5 seconds

  val reporter: ActorRef = actorSystem.actorOf( Props(new Error.Reporter) )
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

  val astFutures = for (file <- commandLine.files) yield driver.poduceAST(file)
  val astResults = for (ast <- astFutures) yield Await.ready(ast, Duration.Inf).value.get
  astResults foreach {
    case Success((_, errors)) =>
      if (errors.exists(_.isFailure)) {
        errors.foreach {
          error =>
            error.recover {
              case e: Error.Error => println(errorFormatter.format(e))
              case _ => println("INTERNAL COMPILER ERROR OCCURRED:"); println(error)
            }
        }
        ErrorExit()
      }
    case Failure(e) =>
      e match {
        case error: Error.Error => println(errorFormatter.format(error))
        case _ => println("INTERNAL COMPILER ERROR OCCURRED:"); println(e)
      }
      ErrorExit()
  }
  val asts: Array[CompilationUnit] = astResults.collect { case Success((ast, _)) => ast }

  // TODO: this should actually divide asts into appropriate packages
  val hierarchy: Map[String, Array[CompilationUnit]] = Map( "Unnamed" -> asts )

  val imnotsurewhatthisshouldbe: Unit = for(ast <- asts) yield driver.translate(hierarchy, ast)

  CleanExit()
}
