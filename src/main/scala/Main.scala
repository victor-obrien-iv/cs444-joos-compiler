import Disambiguator.TypeChecker
import AST.PrettyPrinter
import Assembler.AsmVisitor
import Driver.{CommandLine, Driver}
import Environment.Environment
import Error.ErrorFormatter
import HierarchyChecker.HierarchyChecker
import TypeLinker.{TypeContextBuilder, TypeLinker}
import StaticAnalyzer._

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

  val astFutures = for (file <- commandLine.files) yield driver.produceAST(file)
  val asts = Future.sequence(astFutures.toList)

  val typeContextTry = asts map {
    astList =>
      typeLinker.buildContext(astList)
  }

  val linkedAndChecked = typeContextTry.flatMap {
    typeContext =>
      asts.flatMap { futures =>
        val localContexts = futures.map { ast =>
          ast -> typeLinker.buildLocalContext(ast, typeContext)
        }.toMap

        val linkers = futures.map { ast =>
          val linker = new TypeLinker(localContexts(ast), typeContext)
          linker.run(ast)
        }

        val localContextsTrimmed = localContexts.map { name => name._1 -> name._2.map { decl => decl._1 -> decl._2.head }}
        val checker = new HierarchyChecker(localContextsTrimmed, typeContext)
        val hierarchyCycles = checker.checkForCycles()
        val checkers = futures.flatMap {
          ast => checker.check(ast)
        }
        val hierarchy = hierarchyCycles :: checkers

        val staticAnalysis = futures.flatMap { ast =>
          List(
            new ReturnsPass(ast.fileName).run(ast),
            new InitializationPass(ast.fileName).run(ast)
          )
        }

        val print = futures.map { ast =>
          new PrettyPrinter(ast.fileName).run(ast)
        }

        //TODO: Fix hierarchy checker
        Future.sequence(linkers /*++ hierarchy*/ ++ staticAnalysis ++ print)
      }
  }


  val typeCheck = for {
    typeContext <- typeContextTry
    checked <- linkedAndChecked
    astList <- asts
  } yield {
    val mapLink = astList.map{
      ast =>
        (ast.typeDecl, typeLinker.buildSimpleTypeLink(ast, typeContext))
    }
    val interfaceMethods = typeLinker.buildInterfaces(astList)
    println(interfaceMethods.map(_._2.name))
    astList foreach { ast =>
      val localTypeLink = typeLinker.buildSimpleTypeLink(ast, typeContext)
      val packageName = ast.packageName.map(_.name)
      val environment = Environment(typeContext, localTypeLink, mapLink.toMap, interfaceMethods, packageName.getOrElse(""))
      val typeChecker = new TypeChecker(environment)
      println(ast.fileName)
      typeChecker.build(ast)
      new AsmVisitor(ast, typeChecker).visit(ast) //TODO: change this to .run when things are working
    }
  }

  val done = typeCheck andThen {
    case Failure(exception) => exception match {
      case e: Error.Error => println(errorFormatter.format(e)); e.printStackTrace(); ErrorExit()
      case e: Throwable => println(s"INTERNAL COMPILER ERROR OCCURRED: $e"); e.printStackTrace(); ErrorExit()
    }
    case Success(_) => CleanExit()
  }

  Await.ready(done, Duration.Inf)
}
