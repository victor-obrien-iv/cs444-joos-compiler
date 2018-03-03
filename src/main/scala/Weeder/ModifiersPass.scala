package Weeder

import AST._
import akka.actor.ActorRef

/**
  * A pass to check the following:
  *   A class cannot be both abstract and final
  *   A method has a body if and only if it is neither abstract nor native
  *   An abstract method cannot be static or final
  *   A static method cannot be final
  *   No field can be final
  *   A class must be public
  *   A method must be public or protected
  *
  * this pass analyses:
  *   ClassDecl
  *   FieldDecl
  *   MethodDecl
  */
class ModifiersPass(val fileName: String) extends Visitor {

  // All methods in an interface are implicitly abstract
  var inInterface = false
  override def visit(id: InterfaceDecl): Unit = {
    inInterface = true
    super.visit(id)
    inInterface = false
  }

  // A class cannot be both abstract and final
  override def visit(cd: ClassDecl): Unit = {
    val isAbstract = cd.modifiers.exists(_.isInstanceOf[Token.JavaAbstract])
    val isFinal = cd.modifiers.exists(_.isInstanceOf[Token.JavaFinal])
    val isPublic = cd.modifiers.exists(_.isInstanceOf[Token.JavaPublic])

    if( !isPublic )
      throw Error.Error(cd.name.lexeme, "A class must be public",
        Error.Type.ModifiersPass, Some( Error.Location(cd.name.row, cd.name.col, fileName)))

    if( isAbstract && isFinal )
      throw Error.Error(cd.name.lexeme, "A class cannot be both abstract and final",
        Error.Type.ModifiersPass, Some( Error.Location(cd.name.row, cd.name.col, fileName)))

    super.visit(cd: ClassDecl)
  }

  override def visit(md: MethodDecl): Unit = {
    def error(err: String): Unit = {
      throw Error.Error(md.name.lexeme, err, Error.Type.ModifiersPass,
        Some( Error.Location(md.name.row, md.name.col, fileName)))
    }
    val isAbstract = md.modifiers.exists(_.isInstanceOf[Token.JavaAbstract]) || inInterface
    val isNative = md.modifiers.exists(_.isInstanceOf[Token.JavaNative])
    val isStatic = md.modifiers.exists(_.isInstanceOf[Token.JavaStatic])
    val isFinal = md.modifiers.exists(_.isInstanceOf[Token.JavaFinal])
    val isPublic = md.modifiers.exists(_.isInstanceOf[Token.JavaPublic])
    val isProtected = md.modifiers.exists(_.isInstanceOf[Token.JavaProtected])

    if( !isPublic && !isProtected )
      error("A method must be public or protected")

    // A method has a body if and only if it is neither abstract nor native
    if ( (isAbstract || isNative) && md.body.isDefined )
      error("A method has a body if and only if it is neither abstract nor native")
    else if ( !(isAbstract || isNative) && md.body.isEmpty )
      error("A method has a body if and only if it is neither abstract nor native")

    // An abstract method cannot be static or final
    if ( isAbstract && ( isStatic || isFinal ) )
      error("An abstract method cannot be static or final")

    // A static method cannot be final
    if ( isStatic && isFinal )
      error("A static method cannot be final")

  }

  // No field can be final
  override def visit(fd: FieldDecl): Unit = {
    if ( fd.modifiers.exists(_.isInstanceOf[Token.JavaFinal]) )
      throw Error.Error(fd.name.lexeme, "No field can be final",
        Error.Type.ModifiersPass, Some( Error.Location(fd.name.row, fd.name.col, fileName)))

  }

  override def visit(cd: ConstructorDecl): Unit = {
    val isAbstract = cd.modifiers.exists(_.isInstanceOf[Token.JavaAbstract])
    if ( isAbstract )
      throw Error.Error(fileName /*TODO give ConstructorDecl a name*/, "A constructor cannot be abstract",
        Error.Type.ModifiersPass, None /*TODO Fix this*/)
  }
}
