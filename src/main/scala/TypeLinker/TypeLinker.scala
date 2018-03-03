package TypeLinker

import AST.{ClassType, TypeDecl, Visitor}

class TypeLinker(localContext: Map[String, TypeDecl]) extends Visitor {

  override def visit(ct: ClassType): Unit = {
    if (!localContext.exists(_._1.endsWith(ct.typeID.name))) {
      throw Error.Error(ct.typeID.name, s"Type ${ct.typeID.name} was not found", Error.Type.TypeLinking)
    }
  }

}
