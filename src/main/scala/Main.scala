
import Driver.{CommandLine, Driver}
import Environment.{Environment, EnvironmentBuilder}
import Error.ErrorFormatter
import HierarchyChecker.HierarchyChecker
import TypeLinker.{TypeContextBuilder, TypeLinker}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
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
  val environmentBuilder = new EnvironmentBuilder

  val astFutures = for (file <- commandLine.files) yield driver.produceAST(file)
  val asts = Future.sequence(astFutures.toList)

  val typeContextTry = asts map {
    astList =>
      typeLinker.buildContext(astList)
  }

  val augmentedAst = for {
    context <- typeContextTry
    astList <- asts
  } yield {
    astList.map { ast =>
      val localContext = typeLinker.buildLocalContext(ast, context)
      val environment = Environment(context ++ localContext)
      environmentBuilder.build(ast, environment)
    }
  }

  val linkedAndChecked = typeContextTry.flatMap {
    typeContext =>
      asts.flatMap { futures =>
        val localContexts = futures.map { ast =>
          ast -> typeLinker.buildLocalContext(ast, typeContext)
        }.toMap

        val linkers = futures.map { ast =>
          val linker = new TypeLinker(localContexts(ast), typeContext)
//          val checker = new HierarchyChecker(context, typeContext)
          linker.run(ast) //+: checker.check(ast)
        }

        //     Map[CompilationUnit, Map[String, List[TypeDecl]]]
        // ==> Map[CompilationUnit, Map[String, TypeDecl]]
        val localContextsTrimmed = localContexts.map { name => name._1 -> name._2.map { decl => decl._1 -> decl._2.head }}
        val checker = new HierarchyChecker(localContextsTrimmed, typeContext)

        val hierarchy: Future[Unit] = checker.checkForCycles()

        val checkers: Seq[List[Future[Unit]]] = futures.map {
          ast => checker.check(ast)
        }

        Future.sequence(linkers ++ checkers.flatten :+ hierarchy)
      }
  }

  val createdAst = for {
    augmentedNode <- augmentedAst
    checker <- linkedAndChecked
  } yield {
    augmentedNode.foreach {
      ast =>
        ast.environment.types.foreach {
          case (key, value) =>
            val typeAugmented = value.map(_.name.lexeme)
            println(s"$key: $value")
        }
    }
  }

  val done = createdAst andThen {
    case Failure(exception) => exception match {
      case e: Error.Error => println(errorFormatter.format(e)); ErrorExit()
      case e: Throwable => println(s"INTERNAL COMPILER ERROR OCCURRED: $e"); e.printStackTrace(); ErrorExit()
    }
    case Success(_) => CleanExit()
  }

  Await.ready(done, Duration.Inf)
}
