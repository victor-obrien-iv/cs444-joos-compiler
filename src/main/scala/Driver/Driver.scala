package Driver

import AST.{AstBuilder, CompilationUnit, TypeDecl, Visitor}
import Lalr.{Lalr, LalrReader}
import Parser.{Parser, TreeNode}
import Token.{Comment, Token}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class Driver(implicit ec: ExecutionContext) {
  def produceAST(fileName: String): Future[CompilationUnit] = {

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

    val weeding = ast.flatMap {
      rootNode =>
        val completedNodes: Seq[Future[Unit]] =
          for (weeder <- weeders) yield weeder.run(rootNode)
        val errors = Future.sequence(completedNodes)
        errors.map(_ => rootNode)
    }

    weeding
  }

}
