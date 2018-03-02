package Driver

import java.io.{FileInputStream, ObjectInputStream}

import AST.{AstBuilder, AstNode}
import Lalr.{Lalr, LalrReader}
import Parser.{Parser, TreeNode}
import Token.{Comment, Token}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.Try

class Driver(reporter: ActorRef)(implicit actorSystem: ActorSystem, timeout: Timeout) {
  def compile(fileName: String): Future[(AstNode, List[Try[Unit]])] = {

    // create lexer actor
    val lexer = actorSystem.actorOf( Props(new Lexer.Lexer(actorSystem, reporter)), "Lexer" )

    // give the lexer work
    val tokens: Future[List[Token]] = ask(lexer, fileName).mapTo[List[Token]]

    // instantiate parser actor while lexer works
    val lalrReader = new LalrReader()
    val lalr: Lalr = lalrReader.read("src/main/resources/joos.lr1")
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
      parseTreeNode =>
        builder.build(parseTreeNode)
    }

    val weeding: Future[(AstNode, List[Try[Unit]])] = ast.flatMap {
      rootNode =>
        val completedNodes: List[Future[Try[Unit]]] = for (weeder <- weeders)
        yield ask(weeder, rootNode).mapTo[Try[Unit]]
        val errors = Future.sequence(completedNodes)
        errors.map((rootNode, _))
    }

    weeding
  }
}
