package Weeder

import AST._
import akka.actor.ActorRef


/**
  * A pass to check the following:
  *   A class/interface must be declared in a .java file with the same base name as the class/interface
  *
  * this pass analyses:
  *   CompilationUnit
  *   ClassDecl
  *   InterfaceDecl
  */
class FileNameClassNamePass(val fileName: String, val reporter: ActorRef) extends Visitor {
  def getFileBaseName: String = {
    val lastSlash = fileName.lastIndexWhere((c: Char) => c == '/' || c == '\\')
    val str = fileName.substring(
      if( lastSlash > 0 ) lastSlash + 1 else 0,   // remove the path ahead of the file
      fileName.length - 5)                        // remove the .java from the end
    str
  }
  private val fileBaseName = getFileBaseName

  override def visit(cu: CompilationUnit): Unit = {
    for(i: InterfaceDecl <- cu.interfaces) {
      if ( i.name.lexeme == fileBaseName ) return
    }
    for(cd: ClassDecl <- cu.classes) {
      if ( cd.name.lexeme == fileBaseName ) return
    }

    throw Error.Error(fileName,
      "A class/interface must be declared in a .java file with the same base name as the class/interface",
      Error.Type.Weeder, None)
  }
}
