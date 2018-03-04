package Driver

import AST.{AstBuilder, CompilationUnit, Visitor}
import Lalr.{Lalr, LalrReader}
import Parser.{Parser, TreeNode}
import Token.{Comment, Token}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.Try

class Driver() {
  def produceAST(fileName: String): Future[(CompilationUnit, List[Try[Unit]])] = {

    // create lexer actor


    // give the lexer work
    val tokens: Future[List[Token]] = Future {
      val lexer = new Lexer.Lexer()
      lexer.tokenize(fileName)
    }

    // instantiate parser actor while lexer works
    val lalrReader = new LalrReader()
    val lalr: Lalr = lalrReader.read("src/main/resources/joos.lr1")
    val parser = new Parser(lalr, fileName)
    val builder = new AstBuilder(fileName)
    // create the weeding objects
    val weeders: List[Visitor] = List(
      new Weeder.FileNameClassNamePass(fileName),
      new Weeder.HasConstructorPass(fileName),
      new Weeder.IntegerBoundsPass(fileName),
      new Weeder.ModifiersPass(fileName),
      new Weeder.EnvironmentPass(fileName)
    )

    val parseTree: Future[TreeNode] = tokens.map {
      tokenList =>
        parser.parse(tokenList.filterNot(_.isInstanceOf[Comment]))
    }

    val ast: Future[CompilationUnit] = parseTree.map {
      parseTreeNode => builder.build(parseTreeNode)
    }

    val weeding: Future[(CompilationUnit, List[Try[Unit]])] = ast.flatMap {
      rootNode =>
        val completedNodes: List[Future[Try[Unit]]] =
          for (weeder <- weeders) yield weeder.run(rootNode)
        val errors = Future.sequence(completedNodes)
        errors.map((rootNode, _))
    }

    weeding
  }

  def translate(hierarchy: Map[String, Array[CompilationUnit]], ast: CompilationUnit) = Future {
    /*TODO: this will probably need to return something...*/
    // type linking

    // hierarchy checking
//    val weeders: List[Visitor] = List(
//      // TODO: make the hierarchy weeders
//    )
//
//    val weeding: List[Future[Try[Unit]]] = for(w <- weeders) yield (w ask ast).mapTo[Try[Unit]]

  }
}
