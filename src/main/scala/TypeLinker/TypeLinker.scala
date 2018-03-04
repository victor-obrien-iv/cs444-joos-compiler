package TypeLinker

import AST._

class TypeLinker(localContext: Map[String, List[TypeDecl]], typeContext: Map[String, List[TypeDecl]]) extends Visitor {

  override def visit(ct: ClassType): Unit = checkType(ct.typeID)

  override def visit(obj: ObjNewExpr): Unit = checkType(obj.ctor)

  private def checkType(typeName: FullyQualifiedID): Unit = {
    val packageName = typeName.qualifiers.map(_.lexeme).mkString(".")

    if (typeName.qualifiers.isEmpty) {
      if (localContext.contains(typeName.id.lexeme)) {
        if (localContext(typeName.id.lexeme).lengthCompare(1) == 1) {
          val test = localContext.flatMap{case (a,b) => b map(c => s"$a:${c.name.lexeme}")}
          println(test)
          throw Error.Error(typeName.name, "Multiple packages with type defined", Error.Type.TypeLinking)
        }
        return
      } else typeNotFoundError(typeName)
    }

    if (typeContext.contains(packageName)) {
      if (!typeContext(packageName).exists(_.name.lexeme == typeName.id.lexeme)) {
        typeNotFoundError(typeName)
      }
    } else {
      throw Error.Error(typeName.name, s"Package $packageName was not found", Error.Type.TypeLinking)
    }
  }

  private def typeNotFoundError(id: FullyQualifiedID) = {
    throw Error.Error(id.name, s"Type ${id.name} was not found", Error.Type.TypeLinking)
  }
}
