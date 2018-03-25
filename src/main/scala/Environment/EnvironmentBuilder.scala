package Environment

import AST._
import Token.{Identifier, JavaStatic}

abstract class EnvironmentBuilder[T](environment: Environment) {

  def build(compilationUnit: CompilationUnit,
            typeContext: Map[String, List[TypeDecl]],
            localContext: Map[String, List[TypeDecl]]): T = {
    val CompilationUnit(fileName, packageName, imports, typeDecl) = compilationUnit
    val thisEnv = Environment(typeContext, localContext + ("this" -> List(typeDecl)))
    build(typeDecl, thisEnv)
  }

  def build(typeDecl: TypeDecl, environment: Environment): T

  /**
    * Finds a field in a Class by the given id. Searches through super classes as well
    *
    * @param id The identifier of a field
    * @return The Class the field was found in and the declaration of the field
    */
  def findField(id: Identifier, typeDecl: TypeDecl): (TypeDecl, FieldDecl)

  /**
    * Finds a static field for a Class
    *
    * @param id The identifier of a field
    * @return The declaration of the field
    */
  def findStaticField(id: Identifier, typeDecl: TypeDecl): FieldDecl

  /**
    * Finds the method based on the parameter types and the identifier. Searches
    * through super classes as well
    *
    * @param id Identifier of the method
    * @param parameters The types of the parameters
    * @return The Class the field belongs to (if inherited) and the declaration fo the method
    */
  def findMethod(id: Identifier, parameters: List[Type], typeDecl: TypeDecl): (TypeDecl, MethodDecl) = {
    val memberOption = typeDecl.members.find {
      case MethodDecl(modifiers, returnType, name, parameterDecls, body) =>
        val idMatch = name.lexeme == id.lexeme
        lazy val parameterTypes = parameterDecls.map(_.typ)
        lazy val parametersMatch = parameters == parameterTypes
        idMatch && parametersMatch
      case _ => false
    }

    memberOption match {
      case Some(value) => (typeDecl, value.asInstanceOf[MethodDecl])
      case None =>
        val superClass = typeDecl.superClass match {
          case Some(value) =>
            environment.findType(value)
          case None =>
            environment.findType("java.lang.Object")
        }

        if (typeDecl == superClass) {
          throw Error.Error.memberNotFound(typeDecl.name.lexeme, id)
        }

        findMethod(id, parameters, superClass)
    }
  }

  /**
    * Finds the static method based on the parameters types and the identifier.
    *
    * @param id Identifier of the method
    * @param parameters The types of the parameters
    * @return The declaration of the method
    */
  def findStaticMethod(id: Identifier, parameters: List[Type], typeDecl: TypeDecl): MethodDecl = {
    val (typeOf, method) = findMethod(id, parameters, typeDecl)
    if (method.modifiers.exists(_.isInstanceOf[JavaStatic])) {
      method
    } else {
      throw Error.Error.memberNotFound(typeDecl.name.lexeme, id)
    }
  }

  /**
    * Find the constructor based on the types of the parameters
    *
    * @param parameters The types of the parameters
    * @return The declaration of the constructor
    */
  def findConstructor(parameters: List[Type], typeDecl: TypeDecl): ConstructorDecl = {
    val ctorOption = typeDecl.members.find {
      case ConstructorDecl(modifiers, _, parameterDecls, _) =>
        lazy val parameterTypes = parameterDecls.map(_.typ)
        parameters == parameterTypes
      case _ => false
    }

    ctorOption match {
      case Some(value) => value.asInstanceOf[ConstructorDecl]
      case None => throw Error.Error.memberNotFound(typeDecl.name.lexeme, typeDecl.name)
    }
  }

}
