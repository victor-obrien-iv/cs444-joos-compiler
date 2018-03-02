package Weeder

import AST._
import akka.actor.ActorRef

/**
  * A pass to check the following:
  *   A class must not extend an interface
  *   A class must not extend a final class
  *   An interface must not extend a class
  *
  * this pass analyses:
  *   ClassDecl
  *   InterfaceDecl
  */
class ExtendsPass(hierarchy: Map[String, Array[CompilationUnit]], ast: CompilationUnit) extends Visitor  {

  // A class must not extend an interface
  // A class must not extend a final class
  override def visit(cd: ClassDecl): Unit = {
    cd.extensionOf match {
      case Some(extend: FullyQualifiedID) =>
        for(pack <- hierarchy; comp: CompilationUnit <- pack._2 ) {
          comp.typeDecl match {
            case clas: ClassDecl =>
              if (clas.name.lexeme == extend.id.lexeme) // clas is the class being extended
                if (clas.modifiers.exists(_.isInstanceOf[Token.JavaFinal]))
                  throw Error.Error(extend.id.lexeme, "A class must not extend a final class",
                    Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, cd.name.lexeme)))
                else return
            case interface: InterfaceDecl =>
              if (interface.name.lexeme == extend.id.lexeme)
                throw Error.Error(extend.id.lexeme, "A class must not extend an interface",
                  Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, cd.name.lexeme)))
          }
        }
        assert(assertion = false, "The ExtendsPass could not locate the extended class")
      case None =>
    }
  }

  // An interface must not extend a class
  override def visit(id: InterfaceDecl): Unit = {
    id.extensionOf foreach { extend =>
      for(pack <- hierarchy; comp: CompilationUnit <- pack._2) {
        comp.typeDecl match {
          case clas: ClassDecl =>
            if (clas.name.lexeme == extend.id.lexeme)
              throw Error.Error(extend.id.lexeme, "A class must not extend a final class",
                Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, id.name.lexeme)))
          case _ =>
        }
      }
    }
  }

}
