package Assembler

import AST._
import Driver.FileOperations._
import java.io.PrintWriter

import Environment.Environment

class AsmVisitor(ast: CompilationUnit, environment: Environment) extends Visitor {
  val sFileName = s"output/${getFileBaseName(ast.fileName)}.s"
  val writer = new PrintWriter(sFileName, "UTF-8")
  val assembler = new Assembler(ast, environment)

  def write(instrs: List[String]): Unit = {
    instrs foreach { instr =>
      writer.println(instr)
    }
  }

  override def visit(cu: CompilationUnit): Unit = {
    writer.println("SECTION .text")
    super.visit(cu)
    writer.close()
    println(s"wrote to $sFileName")
  }

  override def visit(md: MethodDecl): Unit = {
    if (md.name.lexeme == "test") write(assembler.assemble(md))
  }
}
