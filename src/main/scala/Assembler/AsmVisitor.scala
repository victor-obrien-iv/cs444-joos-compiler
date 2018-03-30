package Assembler

import AST._
import java.io._
import Driver.FileOperations._

class AsmVisitor(filename: String, id: Int) extends Visitor {
  val sFileName: String = getPath(filename) + "/output/" + getFileBaseName(filename) + ".s"
  val sFile = new File(sFileName)
  val bw = new BufferedWriter(new FileWriter(sFile))
  val assembler = new Assembler(id)

  def write(instrs: List[String]): Unit = {
    instrs foreach { instr =>
      bw.write(instr)
      bw.write('\n')
    }
    bw.write('\n')
  }

  override def visit(cu: CompilationUnit): Unit = {
    bw.write("SECTION .text\n\n")
    super.visit(cu)
    bw.close()
  }

  override def visit(md: MethodDecl): Unit = {
    if (md.name.lexeme == "test")
      md.body match {
        case Some(bodyStmts) =>
          write(assembler.assemble(bodyStmts))
        case None =>
      }

  }
}
