package Environment

import AST._

/**
  * Builds an environment of all the names in scope
  *
  * @param qualifiedTypes Full compiled type context
  * @param types Local type context
  * @param typeContexts The simple type contexts of types
  * @param interfaceMethods All interface methods in the global context
  * @param packageName The environments package
  */
case class Environment(qualifiedTypes: Map[String, List[TypeDecl]] = Map.empty,
                       types: Map[String, String] = Map.empty,
                       typeContexts: Map[TypeDecl, Map[String, String]] = Map.empty,
                       interfaceMethods: List[(InterfaceDecl, MethodDecl)] = Nil,
                       packageName: String = ""
                      ) {

  /**
    * Finds the type based on id, simple or qualified
    *
    * @param id
    * @return
    */
  def findType(id: FullyQualifiedID): Option[TypeDecl] = findType(id.name)

  /**
    * Finds the type based on string version of id
    *
    * @param id
    * @return
    */
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

  /**
    * Finds only based qualified type type, no simple types!!
    *
    * @param id
    * @return
    */
  def findQualifiedTypeDecl(id: String): Option[TypeDecl] = {
    val fullId = id.split('.')
    val pack = fullId.dropRight(1).mkString(".")
    val name = fullId.last

    //Finds type based on full qualified name
    qualifiedTypes.get(pack).flatMap(_.find(_.name.lexeme == name))

  }

  /**
    * Finds the qualified type of a simple type
    *
    * @param id
    * @return
    */
  def findQualifiedType(id: String): Option[String] = {
    if (id.contains('.')) {
      Some(id)
    } else {
      types.get(id)
    }
  }

  /**
    * Finds the type declaration of a simple type as seem in a different type declaration
    *
    * @param id
    * @param typeDecl
    * @return
    */
  def findExternType(id: String, typeDecl: TypeDecl): Option[String] = {
    findQualifiedType(id) match {
      case Some(value) => Some(value)
      case None => typeContexts.get(typeDecl).flatMap(_.get(id))
    }
  }


  /**
    * Checks if the package exists in scope
    *
    * @param packageName package name
    * @return
    */
  def containsPackage(packageName: String): Boolean = {
    qualifiedTypes.keys.exists(_.startsWith(packageName))
  }

}