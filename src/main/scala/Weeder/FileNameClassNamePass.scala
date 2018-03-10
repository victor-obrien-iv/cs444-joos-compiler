package Weeder

import AST._


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
  def fileBaseName: String = {
    val lastSlash = fileName.lastIndexWhere((c: Char) => c == '/' || c == '\\')
    val str = fileName.substring(
      if( lastSlash > 0 ) lastSlash + 1 else 0,   // remove the path ahead of the file
      fileName.length - 5)                        // remove the .java from the end
    str
  }

  override def visit(cu: CompilationUnit): Unit = {
    if (cu.typeDecl.name.lexeme != fileBaseName)
      throw Error.Error(fileName,
        "A class/interface must be declared in a .java file with the same base name as the class/interface",
        Error.Type.Weeder, None)

    super.visit(cu)
  }

  override def visit(cd: ConstructorDecl): Unit = {
    if(cd.identifier.lexeme != fileBaseName)
      throw Error.Error(fileName,
        "A constructor must have the same name as the class/interface",
        Error.Type.Weeder, Some(Error.Location(cd.identifier.row, cd.identifier.col, fileName)))
  }
}
