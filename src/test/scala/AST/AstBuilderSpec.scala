package AST

import Parser.TreeNode
import Token.{JavaReturn, SemiColon}
import org.scalatest.{FlatSpec, Matchers}

class AstBuilderSpec extends FlatSpec with Matchers{

  val blockTest = TreeNode(Right("BlockStatements"),
    List(TreeNode(Right("BlockStatements"), List()), TreeNode(Right("BlockStatement"), List(
      TreeNode(Right("Statement"), List(TreeNode(Right("StatementNoTrailingSubstatement"), List(
        TreeNode(Right("ReturnStatement"), List(TreeNode(Left(JavaReturn("return", 0, 0)), List()),
          TreeNode(Left(SemiColon(";", 0,0)), List())))
      ))))
    ))))

  val builder = new AstBuilder()

  "Builder " should " build goddamn it" in {
    println(builder.buildBlockStatement(blockTest))
    builder.buildBlockStatement(blockTest).stmts.length shouldEqual 5
  }
}

