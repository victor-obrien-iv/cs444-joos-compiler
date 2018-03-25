package Environment

import AST._

/**
  * Builds an environment of all the names in scope
  *
  * @param qualifiedTypes Full compiled type context
  * @param types Local type context
  */
case class Environment(qualifiedTypes: Map[String, List[TypeDecl]] = Map.empty,
                       types: Map[String, List[TypeDecl]] = Map.empty,
                      ) {

  def findType(id: FullyQualifiedID): TypeDecl = {
    if (id.qualifiers.isEmpty) {
      //Finds a simple name in the local type context
      findType(id.name)
    } else {
      //Finds type based on full qualified name
      if (!qualifiedTypes.contains(id.pack)) {
        throw Error.Error.classNotFound(id)
      }

      val packageTypes = qualifiedTypes(id.pack)

      val typeDecl = packageTypes.find(_.name.lexeme == id.id.lexeme)

      typeDecl match {
        case Some(value) => value
        case None => throw Error.Error.classNotFound(id)
      }
    }
  }

  def findType(id: String): TypeDecl = {
    if (!types.contains(id)) {
      throw Error.Error.classNotFound(id)
    }
    if (types(id).lengthCompare(1) > 0) {
      throw Error.Error.multipleTypes(id)
    }
    types(id).head
  }

}

object Environment {
  def empty: Environment = Environment()
}