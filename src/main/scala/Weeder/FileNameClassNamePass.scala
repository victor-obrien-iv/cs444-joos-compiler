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
class FileNameClassNamePass (val fileName: String, val reporter: ActorRef) extends Visitor {
  private val fileBaseName = fileName.substring(0, fileName.length - 5) // remove the .java from the end

  override def visit(cu: CompilationUnit): Unit = {
    for(i: InterfaceDecl <- cu.interfaces) {
      if ( i.name.lexeme == fileBaseName ) return
    }
    for(cd: ClassDecl <- cu.classes) {
      if ( cd.name.lexeme == fileBaseName ) return
    }

    reporter ! Error.Error(fileName,
      "A class/interface must be declared in a .java file with the same base name as the class/interface",
      Error.Type.Weeder, None)
  }
}
