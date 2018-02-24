package Weeder

import AST._
import akka.actor.ActorRef

/**
  * A pass to check the following:
  *   Every class must contain at least one explicit constructor
  *
  * this pass analyses:
  *   CompilationUnit
  *   ClassDecl
  *   ConstructorDecl
  */
class HasConstructorPass(val fileName: String, val reporter: ActorRef) extends Visitor  {

  override def visit(cu: CompilationUnit): Unit = {
    for(cd: ClassDecl <- cu.classes) {
      if( !cd.members.exists(_.isInstanceOf[ConstructorDecl]) )
        throw Error.Error(cd.name.lexeme,
          "Every class must contain at least one explicit constructor",
          Error.Type.Weeder, Some( Error.Location(cd.name.row, cd.name.col, fileName)))
    }
  }
}
