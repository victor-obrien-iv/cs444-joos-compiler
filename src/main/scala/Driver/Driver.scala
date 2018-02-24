package Driver

import java.io.{FileInputStream, ObjectInputStream}

import AST.{AstActor, AstBuilder, AstNode}
import Lalr.Lalr
import Parser.{Parser, TreeNode}
import Token.{Comment, Token}
import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object Driver {
  val actorSystem: ActorSystem = ActorSystem( "actorSystem" )
  val reporter: ActorRef = actorSystem.actorOf( Props(new Error.Reporter), "Reporter" )
  implicit val timeout: Timeout = 5 hour

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
    val weeders: Array[ActorRef] = Array(
      actorSystem.actorOf( Props(new Weeder.FileNameClassNamePass(fileName, reporter)), "FileNameClassNamePass" ),
      actorSystem.actorOf( Props(new Weeder.HasConstructorPass(fileName , reporter)), "HasConstructorPass" ),
      actorSystem.actorOf( Props(new Weeder.IntegerBoundsPass(fileName , reporter)), "IntegerBoundsPass" ),
      actorSystem.actorOf( Props(new Weeder.ModifiersPass(fileName , reporter)), "ModifiersPass" )
    )

    val parseTree: Future[TreeNode] = tokens.map {
      tokenList =>
        parser.parse(tokenList.filterNot(_.isInstanceOf[Comment]))
    }

    val ast: Future[AstNode] = parseTree.map {
      parseTreeNode => builder.build(parseTreeNode)
    }

    ast onComplete {
      case Success(astNode) => CleanExit()
      case Failure(error) => ErrorExit()
    }
  }
}
