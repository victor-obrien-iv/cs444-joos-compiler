package TypeLinker

import AST.{ClassType, TypeDecl, Visitor}

class TypeLinker(localContext: Map[String, TypeDecl], typeContext: Map[String, List[TypeDecl]]) extends Visitor {

  override def visit(ct: ClassType): Unit = {
    val packageName = ct.typeID.qualifiers.map(_.lexeme).mkString(".")
    val existsSimpleType = ct.typeID.qualifiers.isEmpty && localContext.exists(_._1 == ct.typeID.name)
    val existsQualifiedType = if (typeContext.contains(packageName)) {
      typeContext(packageName).exists(_.name.lexeme == ct.typeID.id.lexeme)
    } else {
      throw Error.Error(ct.typeID.name, s"Package $packageName was not found", Error.Type.TypeLinking)
    }

    if (!(existsSimpleType || existsQualifiedType)) {
      throw Error.Error(ct.typeID.name, s"Type ${ct.typeID.name} was not found", Error.Type.TypeLinking)
    }
  }

}
