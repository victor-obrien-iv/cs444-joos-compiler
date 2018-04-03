package Assembler

import AST._
import Driver.FileOperations._
import java.io.PrintWriter
import Disambiguator.TypeChecker

class AsmVisitor(ast: CompilationUnit, tc: TypeChecker) extends Visitor {
  val sFileName = s"output/${getFileBaseName(ast.fileName)}.s"
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
