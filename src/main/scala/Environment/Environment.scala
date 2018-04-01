package Environment

import AST._

/**
  * Builds an environment of all the names in scope
  *
  * @param qualifiedTypes Full compiled type context
  * @param types Local type context
  */
case class Environment(qualifiedTypes: Map[String, List[TypeDecl]] = Map.empty,
                       types: Map[String, String] = Map.empty,
                       typeContexts: Map[TypeDecl, Map[String, String]] = Map.empty,
                       packageName: String = ""
                      ) {

  def findType(id: FullyQualifiedID): Option[TypeDecl] = findType(id.name)

  def findType(id: String): Option[TypeDecl] = {
    val fullId = id.split('.')
    val pack = fullId.dropRight(1).mkString(".")
    val name = fullId.last

    if (pack == "") {
      types.get(id).flatMap(findQualifiedTypeDecl)
    } else {
      //Finds type based on full qualified name
      qualifiedTypes.get(pack).flatMap(_.find(_.name.lexeme == name))
    }
  }

  def findQualifiedTypeDecl(id: String): Option[TypeDecl] = {
    val fullId = id.split('.')
    val pack = fullId.dropRight(1).mkString(".")
    val name = fullId.last

    //Finds type based on full qualified name
    qualifiedTypes.get(pack).flatMap(_.find(_.name.lexeme == name))

  }

  def findQualifiedType(id: String): Option[String] = {
    if (id.contains('.')) {
      Some(id)
    } else {
      types.get(id)
    }
  }

  def findExternType(id: String, typeDecl: TypeDecl): Option[String] = {
    findQualifiedType(id) match {
      case Some(value) => Some(value)
      case None => typeContexts.get(typeDecl).flatMap(_.get(id))
    }
  }


  def containsPackage(id: String): Boolean = {
    qualifiedTypes.keys.exists(_.startsWith(id))
  }

}

object Environment {
  def empty: Environment = Environment()
}