package TypeLinker

import AST.{ClassType, TypeDecl, Visitor}

class TypeLinker(localContext: Map[String, List[TypeDecl]], typeContext: Map[String, List[TypeDecl]]) extends Visitor {

  override def visit(ct: ClassType): Unit = {
    val packageName = ct.typeID.qualifiers.map(_.lexeme).mkString(".")

    if (ct.typeID.qualifiers.isEmpty) {
      val print = localContext.map {
        case (a, b) => b map { c => s"$a:${c.name.lexeme}" }
      }
      //println(s"name: ${ct.typeID.name}\n${print.mkString("\n")}")
      if (localContext.contains(ct.typeID.id.lexeme)) {
        if (localContext(ct.typeID.id.lexeme).lengthCompare(1) == 1) {
          throw Error.Error(ct.typeID.name, "Multiple packages with type defined", Error.Type.TypeLinking)
        }
        return
      } else typeNotFoundError(ct)
    }

    if (typeContext.contains(packageName)) {
      if (!typeContext(packageName).exists(_.name.lexeme == ct.typeID.id.lexeme)) {
        typeNotFoundError(ct)
      }
    } else {
      throw Error.Error(ct.typeID.name, s"Package $packageName was not found", Error.Type.TypeLinking)
    }

  }

  private def typeNotFoundError(ct: ClassType) = {
    println(s"!!!!!${ct.typeID.name}!!!!!")
    val print = localContext.map {
      case (a, b) => b map { c => s"$a:${c.name.lexeme}" }
    }
    println(print.mkString("\n"))
    throw Error.Error(ct.typeID.name, s"Type ${ct.typeID.name} was not found", Error.Type.TypeLinking)
  }
}
