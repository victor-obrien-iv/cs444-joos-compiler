import java.util.concurrent.Executors

import AST.CompilationUnit
import Driver.{CommandLine, Driver}
import Error.ErrorFormatter
import TypeLinker.{TypeContextBuilder, TypeLinker}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object Main extends App {
  val errorFormatter: ErrorFormatter = new ErrorFormatter
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

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
  val astResults: Array[Try[CompilationUnit]] = for (ast <- astFutures) yield Await.ready(ast, Duration.Inf).value.get

  val asts = Future.sequence(astFutures.toList)

  val typeContextTry = asts map {
    astList =>
      typeLinker.buildContext(astList)
  }

  val typeLinked = typeContextTry.flatMap{
    typeContext =>
      asts.flatMap{ futures =>
        val linkers = futures.map { ast =>
          val context = typeLinker.buildLocalContext(ast, typeContext)
          val linker = new TypeLinker(context, typeContext)
          linker.run(ast)
        }
        Future.sequence(linkers)
      }
  }

  typeLinked onComplete  {
    case Failure(exception) => exception match {
          case e: Error.Error => println(errorFormatter.format(e)); ErrorExit()
          case e:Throwable => println(s"INTERNAL COMPILER ERROR OCCURRED: $e"); e.printStackTrace(); ErrorExit()
    }
    case Success(_) => CleanExit()
  }

    // TODO: this should actually divide asts into appropriate packages
    //val hierarchy: Map[String, Array[CompilationUnit]] = Map( "Unnamed" -> asts )

    //val imnotsurewhatthisshouldbe: Unit = for(ast <- asts) yield driver.translate(hierarchy, ast)
}
