package Driver

import java.io.{FileInputStream, ObjectInputStream}

import AST.AstActor

import Lalr.Lalr
import Parser.TreeNode
import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

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

    // give the lexer work
    val tokens: Seq[(String, Future[Any])] = for(f <- commandLine.files ) yield (f, lexer ask f)

    // instantiate parser actor while lexer works
    val lalrSteam = new ObjectInputStream(new FileInputStream("src/main/resources/lalr-obj"))
    val lalr: Lalr = lalrSteam.readObject().asInstanceOf[Lalr]
    val parser = actorSystem.actorOf( Props(new Parser.ParserActor(lalr,
      tokens.head._1 /*TODO: fix this for multiple files*/, reporter)), "Parser" )
    val builder = actorSystem.actorOf(  Props(new AstActor(tokens.head._1, reporter)), "ASTBuilder")
    // create the weeding objects
    val weeders: Array[ActorRef] = Array(
      actorSystem.actorOf( Props(new Weeder.FileNameClassNamePass(tokens.head._1 , reporter)), "FileNameClassNamePass" ),
      actorSystem.actorOf( Props(new Weeder.HasConstructorPass(tokens.head._1 , reporter)), "HasConstructorPass" ),
      actorSystem.actorOf( Props(new Weeder.IntegerBoundsPass(tokens.head._1 , reporter)), "IntegerBoundsPass" ),
      actorSystem.actorOf( Props(new Weeder.ModifiersPass(tokens.head._1 , reporter)), "ModifiersPass" )
    )

    // wait for the lexer to finish
    for( ft <- tokens ) {
      // just print out the tokens for now
//      println(ft._1 + ":")
      val tokenList = Await.result(ft._2, Duration.Inf).asInstanceOf[List[Token.Token]]
//      println(tokenList)
    }

    // wait for the lexer to finish
    for (ft <- tokens) Await.ready(ft._2, Duration.Inf)
    if ( errorsFound ) ErrorExit()
    actorSystem.stop(lexer)

    // give the parser work
    val CSTroot: Seq[Future[Any]] = for( t <- tokens ) yield {
      // get the tokens and filter out comment tokens
      val tokens = Await.result(t._2, Duration.Inf).asInstanceOf[List[Token.Token]].
        filterNot(_.isInstanceOf[Token.Comment])
      parser ask tokens
    }

    // wait for the parser to finish
    for (node <- CSTroot) Await.ready(node, Duration.Inf)
    if ( errorsFound ) ErrorExit()

    // give the ast builder work
    val ASTroot = for(node <- CSTroot) yield {
      val treeNode = Await.result(node, Duration.Inf).asInstanceOf[TreeNode]
      // just print out the nodes for now
//      println()
//      println(treeNode)
      builder ask treeNode
    }

    // wait for the ast builder to finish
    val astnode = for(node <- ASTroot) yield {
      Await.result(node, Duration.Inf)
//      println(node)
    }
    if ( errorsFound ) ErrorExit()

    // give the weeders work
    val finished = for( w <- weeders; n <- astnode ) yield w ask n
    for ( f <- finished ) Await.ready(f, Duration.Inf)
    if ( errorsFound ) ErrorExit()

    CleanExit()
  }
}
