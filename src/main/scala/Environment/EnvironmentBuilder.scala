package Environment

import AST._
import Error.Error
import Token._

abstract class EnvironmentBuilder[T](environment: Environment) {

  def build(compilationUnit: CompilationUnit): T = {
    val CompilationUnit(fileName, packageName, imports, typeDecl) = compilationUnit
    build(typeDecl, environment)
  }

  def build(typeDecl: TypeDecl, environment: Environment): T

  protected def partitionMembers(decls: List[MemberDecl]): (List[FieldDecl], List[MethodDecl], List[ConstructorDecl]) =
    decls.foldRight((List.empty[FieldDecl], List.empty[MethodDecl], List.empty[ConstructorDecl])) {
      case (fieldDecl: FieldDecl, (fields, methods, ctors)) => (fieldDecl :: fields, methods, ctors)
      case (methodDecl: MethodDecl, (fields, methods, ctors)) => (fields, methodDecl :: methods, ctors)
      case (ctorDecl: ConstructorDecl, (fields, methods, ctors)) => (fields, methods, ctorDecl :: ctors)
    }

  protected def parametersMatch(paramTypes: List[Type], parameters: List[ParameterDecl]): Boolean = {
    (parameters, paramTypes) match {
      case (Nil, Nil) => true
      case (typeList1, typeList2) if typeList1.lengthCompare(typeList2.length) != 0 => false
      case (typeList1, typeList2) =>
        val paramPairs = typeList1.map(_.typ).zip(typeList2)
        val matches = paramPairs.map{
          case (type1, type2) => typeEquals(type1, type2)
        }
        matches.reduce(_ && _)
    }
  }

  def typeEquals(type1: Type, type2: Type): Boolean = {
    (type1, type2) match {
      case (PrimitiveType(p1), PrimitiveType(p2)) => p1.lexeme == p2.lexeme
      case (ClassType(c1), ClassType(c2)) => environment.findType(c1) == environment.findType(c2)
      case (ArrayType(a1, _), ArrayType(a2, _)) => typeEquals(a1, a2)
      case (NullType(), NullType()) => true
      case _ => false
    }
  }

  def typeAssignable(type1: Type, type2: Type): Boolean = {
    if (typeEquals(type1, type2)) true
    else {
      type1 match {
        case _: ReferenceType =>
          (type1, type2) match {
            case (_, NullType()) => true
            case (ClassType(iD), _:ArrayType) =>
              val classTypeDecl = environment.findType(iD)
              classTypeDecl match {
                case Some(value) =>
                  val typeName = value.name.lexeme
                  typeName == "Object" || typeName == "Serializable" || typeName == "Cloneable"
                case None => throw Error.langLibraryNotLoaded
              }
            case (ArrayType(arrayOf1, _), ArrayType(arrayOf2, _)) =>
              (arrayOf1, arrayOf2) match {
                case (ClassType(iD1), ClassType(iD2)) =>
                  isSubTypeOf(iD1, iD2)
                case (t1, t2) => t1 == t2
              }
            case (ClassType(iD1), ClassType(iD2)) =>
              isSubTypeOf(iD1, iD2)
            case _ => false
          }
        case p: PrimitiveType if p.isNumeric =>
          (type1, type2) match {
            case (PrimitiveType(_: JavaShort), PrimitiveType(_: JavaByte))
                 | (PrimitiveType(_: JavaChar), PrimitiveType(_: JavaShort))
                 | (PrimitiveType(_: JavaShort), PrimitiveType(_: JavaChar))
                 | (PrimitiveType(_:JavaInt), PrimitiveType(_: JavaShort))
                 | (PrimitiveType(_:JavaInt), PrimitiveType(_: JavaChar))
                 | (PrimitiveType(_:JavaInt), PrimitiveType(_: JavaByte)) => true
            case _ => false
          }
      }
    }
  }

  def isSubTypeOf(superType: FullyQualifiedID, subType: FullyQualifiedID): Boolean = {
    if (superType.id.lexeme == "Object") {
      true
    } else if (subType.id.lexeme == "Object") {
      false
    } else {
      val superTypeDecl = environment.findType(superType).getOrElse(throw Error.classNotFound(superType))
      val subTypeDecl = environment.findType(subType).getOrElse(throw Error.classNotFound(subType))
      isSubTypeOf(subTypeDecl, subTypeDecl)
    }
  }

  def isSubTypeOf(superTypeDecl: TypeDecl, subTypeDecl: TypeDecl): Boolean = {
    if (superTypeDecl == subTypeDecl) {
      true
    } else if (subTypeDecl.name.lexeme == "Object") {
      false
    }else {
      val superClass = getSuperClass(subTypeDecl)
      lazy val interfaces = subTypeDecl.superInterfaces.foldRight(false) {
        case (interface, isSub) =>
          val interfaceDecl = environment.findType(interface).getOrElse(throw Error.classNotFound(interface))
          isSubTypeOf(interfaceDecl, subTypeDecl) || isSub
      }
      isSubTypeOf(superTypeDecl, superClass) || interfaces
    }
  }

  def hasProtectedAccess(owner: FullyQualifiedID, ownerDecl: TypeDecl, accessorDecl: TypeDecl): Boolean = {
    if (owner.pack == environment.packageName) {
      true
    } else isSubTypeOf(ownerDecl, accessorDecl)
  }

  /**
    * Finds a field in a Class by the given id. Searches through super classes as well
    *
    * @param id The identifier of a fields
    * @return The Class the field was found in and the declaration of the field
    */
  def findField(id: Identifier, typeDecl: TypeDecl): Option[(TypeDecl, FieldDecl)] = {
    val (fields, _, _) = partitionMembers(typeDecl.members)
    val fieldOption = fields.find {
      field =>
        field.name.lexeme == id.lexeme
    }

    fieldOption match {
      case Some(value) => Some(typeDecl, value)
      case None =>
        val superClass = getSuperClass(typeDecl)

        if (typeDecl == superClass) {
          None
        } else {
          findField(id, superClass)
        }
    }
  }

  /**
    * Finds a static field for a Class
    *
    * @param id The identifier of a field
    * @return The declaration of the field
    */
  def findStaticField(id: Identifier, typeDecl: TypeDecl): Option[(TypeDecl, FieldDecl)] = {
    val fieldOption = findField(id, typeDecl)
    fieldOption.filter(_._2.modifiers.exists(_.isInstanceOf[JavaStatic]))
  }

  def findNonStaticField(id: Identifier, typeDecl: TypeDecl): Option[(TypeDecl, FieldDecl)] = {
    val fieldOption = findField(id, typeDecl)
    fieldOption.filter(!_._2.modifiers.exists(_.isInstanceOf[JavaStatic]))
  }

  /**
    * Finds the method based on the parameter types and the identifier. Searches
    * through super classes as well
    *
    * @param id Identifier of the method
    * @param parameters The types of the parameters
    * @return The Class the field belongs to (if inherited) and the declaration fo the method
    */
  def findMethod(id: Identifier, parameters: List[Type], typeDecl: TypeDecl): Option[(TypeDecl, MethodDecl)] = {
    val (_, methods, _) = partitionMembers(typeDecl.members)
    val memberOption = methods.find {
      case MethodDecl(modifiers, returnType, name, parameterDecls, body) =>
        val idMatch = name.lexeme == id.lexeme
        val parametersEqual = parametersMatch(parameters, parameterDecls)
        idMatch && parametersEqual
    }

    memberOption match {
      case Some(value) => Some((typeDecl, value))
      case None =>
        val superClass = getSuperClass(typeDecl)

        if (typeDecl == superClass) {
          None
        } else {
          findMethod(id, parameters, superClass)
        }
    }
  }

  private def getSuperClass(typeDecl: TypeDecl): TypeDecl = {
    typeDecl.superClass match {
      case Some(value) =>
        environment.findExternType(value, typeDecl).flatMap(environment.findType) match {
          case Some(superClassDecl) => superClassDecl
          case None => throw Error.classNotFound(value.name)
        }
      case None =>
        environment.findType("Object") match {
          case Some(value) => value
          case None => throw Error.langLibraryNotLoaded
        }
    }
  }

  /**
    * Finds the static method based on the parameters types and the identifier.
    *
    * @param id Identifier of the method
    * @param parameters The types of the parameters
    * @return The declaration of the method
    */
  def findStaticMethod(id: Identifier, parameters: List[Type], typeDecl: TypeDecl): Option[(TypeDecl, MethodDecl)] = {
    val methodOption = findMethod(id, parameters, typeDecl)
    methodOption filter {
      case (typeOf, method) =>
        method.modifiers.exists(_.isInstanceOf[JavaStatic])
    }
  }

  def findNonStaticMethod(id: Identifier, parameters: List[Type], typeDecl: TypeDecl): Option[(TypeDecl, MethodDecl)] = {
    val methodOption = findMethod(id, parameters, typeDecl)
    methodOption filter {
      case (typeOf, method) =>
        !method.modifiers.exists(_.isInstanceOf[JavaStatic])
    }
  }

  /**
    * Find the constructor based on the types of the parameters
    *
    * @param parameters The types of the parameters
    * @return The declaration of the constructor
    */
  def findConstructor(parameters: List[Type], typeDecl: TypeDecl): Option[ConstructorDecl] = {
    val (_, _ , ctors) = partitionMembers(typeDecl.members)
    ctors.find {
      case ConstructorDecl(modifiers, _, parameterDecls, _) =>
        parametersMatch(parameters, parameterDecls)
      case _ => false
    }
  }

}
