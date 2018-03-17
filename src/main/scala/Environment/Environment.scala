package Environment

import AST._

/**
  * Builds an environment of all the names in scope
  *
  * @param qualifiedTypes Full compiled type context
  * @param types Local type context
  * @param variables Variables/Parameters in scope
  * @param methods Methods in scope
  * @param constructors Constructors in scope
  */
case class Environment(qualifiedTypes: Map[String, List[TypeDecl]] = Map.empty,
                       types: Map[String, List[TypeDecl]] = Map.empty,
                       variables: Map[String, (Type, Option[Expr])] = Map.empty,
                       staticFields: Map[String, (Type, Option[Expr])] = Map.empty,
                       fields: Map[String, (Type, Option[Expr])] = Map.empty,
                       methods: Map[MethodHeader, (Option[Type], Option[BlockStmt])] = Map.empty,
                       constructors: Map[ConstructorHeader, BlockStmt] = Map.empty) {

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


  def findVar(id: FullyQualifiedID): (Type, Option[Expr]) = {
    findVar(id.name)
  }

  def findVar(str: String): (Type, Option[Expr]) = {
    variables(str)
  }

  def allFields: Map[String, (Type, Option[Expr])] = {
    staticFields ++ fields
  }

  def ++(environment: Environment): Environment = {

    Environment(
      qualifiedTypes ++ environment.qualifiedTypes,
      types ++ environment.types,
      variables ++ environment.variables,
      staticFields ++ environment.staticFields,
      fields ++ environment.fields,
      methods ++ environment.methods,
      constructors ++ environment.constructors
    )
  }
}

object Environment {
  def empty: Environment = Environment()
}