package Driver

import java.io.{FileInputStream, ObjectInputStream}

import AST.{AstBuilder, AstNode}
import Error.{ErrorFormatter, Reporter}
import Lalr.Lalr
import Parser.{Parser, TreeNode}
import Token.{Comment, Token}
import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.collection.mutable.ArrayOps
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

object Driver {
  val actorSystem: ActorSystem = ActorSystem( "actorSystem" )
  val reporter: ActorRef = actorSystem.actorOf( Props(new Error.Reporter), "Reporter" )
  val errorFormatter = new ErrorFormatter
  implicit val timeout: Timeout = 5 seconds

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

    val fileName = commandLine.files.head
    // give the lexer work
    val tokens: Future[List[Token]] = ask(lexer, fileName).mapTo[List[Token]]

    // instantiate parser actor while lexer works
    val lalrSteam = new ObjectInputStream(new FileInputStream("src/main/resources/lalr-obj"))
    val lalr: Lalr = lalrSteam.readObject().asInstanceOf[Lalr]
    val parser = new Parser(lalr, fileName)
    val builder = new AstBuilder(fileName)
    // create the weeding objects
    val weeders: List[ActorRef] = List(
      actorSystem.actorOf( Props(new Weeder.FileNameClassNamePass(fileName)), "FileNameClassNamePass" ),
      actorSystem.actorOf( Props(new Weeder.HasConstructorPass(fileName)), "HasConstructorPass" ),
      actorSystem.actorOf( Props(new Weeder.IntegerBoundsPass(fileName)), "IntegerBoundsPass" ),
      actorSystem.actorOf( Props(new Weeder.ModifiersPass(fileName)), "ModifiersPass" )
    )

    val parseTree: Future[TreeNode] = tokens.map {
      tokenList =>
        parser.parse(tokenList.filterNot(_.isInstanceOf[Comment]))
    }

    val ast: Future[AstNode] = parseTree.map {
      parseTreeNode => builder.build(parseTreeNode)
    }

    val weeding: Future[List[Try[Any]]] = ast.flatMap {
      rootNode =>
        val completedNodes: List[Future[Try[Unit]]] = for (weeder <- weeders)
        yield ask(weeder, rootNode).mapTo[Try[Unit]]
        Future.sequence(completedNodes)
    }

    weeding onComplete {
      case Success(weedings) =>
        val failed = weedings.filter(_.isFailure)
        for (error <- failed)
          error.recover {
            case error: Error.Error => println(errorFormatter.format(error))
          }

        if (failed.nonEmpty) ErrorExit() else CleanExit()
      case Failure(error) => ErrorExit()
    }
  }
}
