
import Driver.{CommandLine, Driver}
import Error.ErrorFormatter
import TypeLinker.{TypeContextBuilder, TypeLinker}
import HierarchyChecker.HierarchyChecker

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.{Failure, Success}

object Main extends App {
  val errorFormatter: ErrorFormatter = new ErrorFormatter

  def ErrorExit(): Unit = {
    println("exit: 42")
    System.exit(42) // the input file is not lexically/syntactically valid Joos 1W
  }

  def CleanExit(): Unit = {
    println("exit: 0")
    System.exit(0) // the input file is lexically/syntactically valid Joos 1W
  }

  val commandLine = new CommandLine(args, errorFormatter)
  val driver = new Driver()
  val typeLinker = new TypeContextBuilder

  val astFutures = for (file <- commandLine.files) yield driver.produceAST(file)
  val asts = Future.sequence(astFutures.toList)

  val typeContextTry = asts map {
    astList =>
      typeLinker.buildContext(astList)
  }

  val linkedAndChecked = typeContextTry.flatMap {
    typeContext =>
      asts.flatMap { futures =>
        val linkAndCheck = futures.map { ast =>
          val context = typeLinker.buildLocalContext(ast, typeContext)
          val linker = new TypeLinker(context, typeContext)
          val checker = new HierarchyChecker(context, typeContext)
          linker.run(ast) +: checker.check(ast)
        }
        Future.sequence(linkAndCheck.flatten)
      }
  }

  val done = linkedAndChecked andThen {
    case Failure(exception) => exception match {
      case e: Error.Error => println(errorFormatter.format(e)); ErrorExit()
      case e: Throwable => println(s"INTERNAL COMPILER ERROR OCCURRED: $e"); ErrorExit()
    }
    case Success(_) => CleanExit()
  }

  Await.ready(done, Duration.Inf)
}
