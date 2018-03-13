package HierarchyChecker

import AST._

import scala.collection.mutable.ListBuffer

/**
  * A pass to check the following:
  *   A class must not extend an interface
  *   A class must not extend a final class
  *   An interface must not extend a class
  *   A class must not implement a class
  *   An interface must not be repeated in an implements clause
  *   An interface must not be repeated in an extends clause of an interface
  *
  * this pass analyses:
  *   ClassDecl
  *   InterfaceDecl
  */
class ExtendsPass(localContext: Map[String, TypeDecl], typeContext: Map[String, List[TypeDecl]],
                  ast: CompilationUnit) extends Visitor  {

  def findInPack(name: String, types: List[TypeDecl]): TypeDecl ={
    for(t <- types)
      if(t.name.lexeme == name)
        return t
    assert(assertion = false, s"Type $name does not exist in hierarchy"); types.head
  }

  // A class must not extend an interface
  // A class must not extend a final class
  // A class must not implement a class
  // An interface must not be repeated in an implements clause
  override def visit(cd: ClassDecl): Unit = {
    cd.extensionOf match {
      case Some(extend: FullyQualifiedID) =>
        val extendType: TypeDecl =
          if ( extend.qualifiers.nonEmpty )
            findInPack(extend.id.lexeme, typeContext(extend.pack))
          else
            localContext(extend.id.lexeme)

        extendType match {
          case _: InterfaceDecl =>
            throw Error.Error(extend.id.lexeme, "A class must not extend an interface",
              Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, ast.fileName)))

          case c: ClassDecl =>
            val isFinal = c.modifiers.exists(_.isInstanceOf[Token.JavaFinal])
            if ( isFinal )
              throw Error.Error(extend.id.lexeme, "A class must not extend a final class",
                Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, ast.fileName)))
        }
      case None =>
    }

    val seen: ListBuffer[Int] = ListBuffer()

    for(imple <- cd.implementationOf) {
      val impleType: TypeDecl =
        if ( imple.qualifiers.nonEmpty )
          findInPack(imple.id.lexeme, typeContext(imple.pack))
        else
          localContext(imple.id.lexeme)

      seen += impleType.id

      impleType match {
        case _: InterfaceDecl =>
        case _: ClassDecl => // do nothing
          throw Error.Error(imple.id.lexeme, "A class must not implement a class",
            Error.Type.ExtendsPass, Some(Error.Location(imple.id.row, imple.id.col, ast.fileName)))
      }
    }

    if( seen.toSet.size != seen.size )
      throw Error.Error(cd.implementationOf.mkString(", "),
        "An interface must not be repeated in an implements clause",
        Error.Type.ExtendsPass, Some(Error.Location(cd.name.row, cd.name.col, ast.fileName)))
  }

  // An interface must not extend a class
  // An interface must not be repeated in an extends clause of an interface
  override def visit(id: InterfaceDecl): Unit = {
    val seen: ListBuffer[Int] = ListBuffer()

    for( extend <- id.extensionOf ) {
      val extendType: TypeDecl =
        if ( extend.qualifiers.nonEmpty )
          findInPack(extend.id.lexeme, typeContext(extend.pack))
        else
          localContext(extend.id.lexeme)

      seen += extendType.id

      extendType match {
        case _: InterfaceDecl => // do nothing
        case _: ClassDecl =>
          throw Error.Error(extend.id.lexeme, "An interface must not extend a class",
            Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, ast.fileName)))
      }
    }

    if( seen.toSet.size != seen.size )
      throw Error.Error(id.extensionOf.mkString(", "),
        "An interface must not be repeated in an extends clause of an interface",
        Error.Type.ExtendsPass, Some(Error.Location(id.name.row, id.name.col, ast.fileName)))
  }

}
