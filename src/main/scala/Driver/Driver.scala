package Driver

import java.io.{FileInputStream, ObjectInputStream}

import AST.{AstBuilder, AstNode}
import Lalr.Lalr
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
  def poduceAST(fileName: String): Future[(AstNode, List[Try[Unit]])] = {

    // create lexer actor
    val lexer = actorSystem.actorOf( Props(new Lexer.Lexer(actorSystem, reporter)) )

    // give the lexer work
    val tokens: Future[List[Token]] = ask(lexer, fileName).mapTo[List[Token]]

    // instantiate parser actor while lexer works
    val lalrSteam = new ObjectInputStream(new FileInputStream("src/main/resources/lalr-obj"))
    val lalr: Lalr = lalrSteam.readObject().asInstanceOf[Lalr]
    val parser = new Parser(lalr, fileName)
    val builder = new AstBuilder(fileName)
    // create the weeding objects
    val weeders: List[ActorRef] = List(
      actorSystem.actorOf( Props(new Weeder.FileNameClassNamePass(fileName)) ),
      actorSystem.actorOf( Props(new Weeder.HasConstructorPass(fileName)) ),
      actorSystem.actorOf( Props(new Weeder.IntegerBoundsPass(fileName)) ),
      actorSystem.actorOf( Props(new Weeder.ModifiersPass(fileName)) ),
      actorSystem.actorOf( Props(new Weeder.EnvironmentPass(fileName)) )
    )

    val parseTree: Future[TreeNode] = tokens.map {
      tokenList =>
        parser.parse(tokenList.filterNot(_.isInstanceOf[Comment]))
    }

    val ast: Future[AstNode] = parseTree.map {
      parseTreeNode => builder.build(parseTreeNode)
    }

    val weeding: Future[(AstNode, List[Try[Unit]])] = ast.flatMap {
      rootNode =>
        val completedNodes: List[Future[Try[Unit]]] =
          for (weeder <- weeders) yield ask(weeder, rootNode).mapTo[Try[Unit]]
        val errors = Future.sequence(completedNodes)
        errors.map((rootNode, _))
    }

    weeding
  }

  def translate(packages: Map[String, Array[AstNode]], ast: AstNode) = Future /*TODO: this will probably need to return something...*/ {
    // type linking

    // hierarchy checking
    val weeders: List[ActorRef] = List(
      // TODO: make the hierarchy weeders
    )

    val weeding: List[Future[Try[Unit]]] = for(w <- weeders) yield (w ask ast).mapTo[Try[Unit]]

  }
}
