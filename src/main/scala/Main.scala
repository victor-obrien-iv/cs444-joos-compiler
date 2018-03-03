import AST.CompilationUnit
import Driver.{CommandLine, Driver}
import Error.ErrorFormatter
import TypeLinker.{TypeContextBuilder, TypeLinker}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

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
  val astResults = for (ast <- astFutures) yield Await.ready(ast, Duration.Inf).value.get
  astResults foreach {
    case Success((_, errors)) =>
      if (errors.exists(_.isFailure)) {
        errors.foreach {
          error: Try[Unit] =>
            error.recover {
              case e: Error.Error => println(errorFormatter.format(e))
                //TODO: Add debug mode to print stacktraces
                //error.printStackTrace()
              case e: Exception => println("INTERNAL COMPILER ERROR OCCURRED:"); println(e.printStackTrace())
            }
        }
        ErrorExit()
      }
    case Failure(e) =>
      e match {
        case error: Error.Error => println(errorFormatter.format(error))
          //TODO: Add debug mode to print stacktraces
          //error.printStackTrace()
        case _ => println("INTERNAL COMPILER ERROR OCCURRED:"); e.printStackTrace()
      }
      ErrorExit()
  }

  val asts: Array[CompilationUnit] = astResults.collect { case Success((ast, _)) => ast }
  val typeContextTry = Try {
    typeLinker.buildContext(asts.toList)
  }

//  val typeLinked = typeContextTry.map{
//    typeContext =>
//      val linkedAsts = asts.map { ast =>
//        val context = typeLinker.buildLocalContext(ast, typeContext)
//        val linker = actorSystem.actorOf(Props(new TypeLinker(context)))
//        ask(linker, ast).mapTo[Try[Unit]]
//      }
//      Future.sequence(linkedAsts.toList)
//  }

//  typeLinked match {
//    case Failure(exception) => exception match {
//          case e: Error.Error => println(errorFormatter.format(e)); ErrorExit()
//          case e:Throwable => println(s"INTERNAL COMPILER ERROR OCCURRED: $e"); e.printStackTrace(); ErrorExit()
//    }
//    case Success(value) => value onComplete {
//      case Failure(exception) => exception match {
//        case e: Error.Error => println(errorFormatter.format(e)); ErrorExit()
//        case e:Throwable => println(s"INTERNAL COMPILER ERROR OCCURRED: $e"); e.printStackTrace(); ErrorExit()
//      }
//      case Success(tryList) => tryList foreach {
//        case Success(tryValue) =>
//        case Failure(exception) => exception match {
//          case e: Error.Error => println(errorFormatter.format(e)); ErrorExit()
//          case e:Throwable => println(s"INTERNAL COMPILER ERROR OCCURRED: $e"); e.printStackTrace(); ErrorExit()
//        }
//      }
//    }
//  }
//  }.recover {

//  }

    // TODO: this should actually divide asts into appropriate packages
    //val hierarchy: Map[String, Array[CompilationUnit]] = Map( "Unnamed" -> asts )

    //val imnotsurewhatthisshouldbe: Unit = for(ast <- asts) yield driver.translate(hierarchy, ast)

  CleanExit()
}
