package Weeder

import AST._
import Driver.FileOperations._


/**
  * A pass to check the following:
  *   A class/interface must be declared in a .java file with the same base name as the class/interface
  *
  * this pass analyses:
  *   CompilationUnit
  *   ClassDecl
  *   InterfaceDecl
  */
class FileNameClassNamePass(val fileName: String) extends Visitor {

  private val fileBaseName = getFileBaseName(fileName)

  override def visit(cu: CompilationUnit): Unit = {
    if (cu.typeDecl.name.lexeme != fileBaseName) {
      throw Error.Error(fileName,
        "A class/interface must be declared in a .java file with the same base name as the class/interface",
        Error.Type.Weeder, None)
    }
  }
}
