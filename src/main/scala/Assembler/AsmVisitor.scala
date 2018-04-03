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

  def write(instrs: List[String]): Unit = {
    instrs foreach { instr =>
      writer.println(instr)
    }
  }

  override def visit(cu: CompilationUnit): Unit = {
//    writer.println("SECTION .text")
    write(assembler.assemble())
    writer.close()
    println(s"wrote to $sFileName")
  }

  override def visit(md: MethodDecl): Unit = {
    if (md.name.lexeme == "test") write(assembler.assemble(md))
  }
}
