package HierarchyChecker

import AST._

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
                Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, cd.name.lexeme)))
        }
      case None =>
    }
  }

  // An interface must not extend a class
  override def visit(id: InterfaceDecl): Unit = {
    for( extend <- id.extensionOf ) {
      val extendType: TypeDecl =
        if ( extend.qualifiers.nonEmpty )
          findInPack(extend.id.lexeme, typeContext(extend.pack))
        else
          localContext(extend.id.lexeme)

      extendType match {
        case _: InterfaceDecl => // do nothing
        case _: ClassDecl =>
          throw Error.Error(extend.id.lexeme, "A class must not extend a final class",
            Error.Type.ExtendsPass, Some(Error.Location(extend.id.row, extend.id.col, ast.fileName)))
      }
    }
  }

}
