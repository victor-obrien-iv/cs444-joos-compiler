package Assembler

import AST._
import Driver.FileOperations._
import java.io.PrintWriter
import Disambiguator.TypeChecker

class AsmVisitor(ast: CompilationUnit, tc: TypeChecker) extends Visitor {
  val packageName = ast.packageName.map(_.name)
  val outPutFile = packageName match {
    case Some(value) => s"$value.${getFileBaseName(ast.fileName)}"
    case None => getFileBaseName(ast.fileName)
  }
  val sFileName = s"output/$outPutFile.s"
  val writer = new PrintWriter(sFileName, "UTF-8")
  val assembler = new Assembler(ast, tc)

  override def visit(cu: CompilationUnit): Unit = {
    assembler.assemble() foreach {
      writer.println(_)
    }
    writer.close()
    println(s"wrote to $sFileName")
  }
}
