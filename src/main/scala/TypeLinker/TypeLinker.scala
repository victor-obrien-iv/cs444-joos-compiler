package TypeLinker

import AST.{ClassType, TypeDecl, Visitor}

class TypeLinker(localContext: List[(String, TypeDecl)], typeContext: Map[String, List[TypeDecl]]) extends Visitor {

  override def visit(ct: ClassType): Unit = {
    val packageName = ct.typeID.qualifiers.mkString(".")
    val notExistsSimpleType = ct.typeID.qualifiers.isEmpty && !localContext.exists(_._1 == ct.typeID.name)
    val notExistsQualifiedType = if (typeContext.contains(packageName)) {
      typeContext(packageName).exists(_.name.lexeme == ct.typeID.name)
    } else {
      throw Error.Error(ct.typeID.name, s"Package $packageName was not found", Error.Type.TypeLinking)
    }

    if (notExistsSimpleType || notExistsQualifiedType) {
      throw Error.Error(ct.typeID.name, s"Type ${ct.typeID.name} was not found", Error.Type.TypeLinking)
    }
  }

}
