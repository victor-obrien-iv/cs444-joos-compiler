package Environment

import AST._
import Disambiguator._
import Error.Error
import Token._

class EnvironmentBuilder[T](environment: Environment) {

  /**
    * Partitions the members of a type to Fields, Methods and Constructors
    *
    * @param decls
    * @return
    */
  protected def partitionMembers(decls: List[MemberDecl]): (List[FieldDecl], List[MethodDecl], List[ConstructorDecl]) =
    decls.foldRight((List.empty[FieldDecl], List.empty[MethodDecl], List.empty[ConstructorDecl])) {
      case (fieldDecl: FieldDecl, (fields, methods, ctors)) => (fieldDecl :: fields, methods, ctors)
      case (methodDecl: MethodDecl, (fields, methods, ctors)) => (fields, methodDecl :: methods, ctors)
      case (ctorDecl: ConstructorDecl, (fields, methods, ctors)) => (fields, methods, ctorDecl :: ctors)
    }

  /**
    * Checks if a list of types match with the parameter declarations
    *
    * @param paramTypes
    * @param parameters
    * @return
    */
  protected def parametersMatch(paramTypes: List[Type], parameters: List[ParameterDecl]): Boolean = {
    (parameters, paramTypes) match {
      case (Nil, Nil) => true
      case (typeList1, typeList2) if typeList1.lengthCompare(typeList2.length) != 0 => false
      case (typeList1, typeList2) =>
        val paramPairs = typeList1.map(_.typ).zip(typeList2)
        val matches = paramPairs.map {
          case (type1, type2) => typeEquals(type1, type2)
        }
        matches.reduce(_ && _)
    }
  }

  /**
    * Checks if two types are equal
    * @param type1
    * @param type2
    * @return
    */
  def typeEquals(type1: Type, type2: Type): Boolean = {
    (type1, type2) match {
      case (PrimitiveType(_: JavaVoid), _) => false
      case (PrimitiveType(p1), PrimitiveType(p2)) => p1.lexeme == p2.lexeme
      case (ClassType(c1), ClassType(c2)) => environment.findType(c1) == environment.findType(c2)
      case (ArrayType(a1, _), ArrayType(a2, _)) => typeEquals(a1, a2)
      case (NullType(), NullType()) => true
      case _ => false
    }
  }

  /**
    * Checks if type2 is type assignable to type1
    * @param type1 The type assigned to
    * @param type2 The type assignee
    * @return True if type assignable, false if not
    */
  def typeAssignable(type1: Type, type2: Type): Boolean = {
    if (typeEquals(type1, type2)) true
    else {
      type1 match {
        case _: ReferenceType =>
          (type1, type2) match {
            case (_, NullType()) => true
            case (ClassType(iD), _: ArrayType) =>
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
                 | (PrimitiveType(_: JavaInt), PrimitiveType(_: JavaShort))
                 | (PrimitiveType(_: JavaInt), PrimitiveType(_: JavaChar))
                 | (PrimitiveType(_: JavaInt), PrimitiveType(_: JavaByte)) => true
            case _ => false
          }
        case _ => false
      }
    }
  }

  /**
    * Checks whether subType is a subType of superType
    *
    * @param superType
    * @param subType
    * @return True/False
    */
  def isSubTypeOf(superType: FullyQualifiedID, subType: FullyQualifiedID): Boolean = {
    if (superType.id.lexeme == "Object") {
      true
    } else if (subType.id.lexeme == "Object") {
      false
    } else {
      val superTypeDecl = environment.findType(superType).getOrElse(throw Error.classNotFound(superType))
      val subTypeDecl = environment.findType(subType).getOrElse(throw Error.classNotFound(subType))
      isSubTypeOf(superTypeDecl, subTypeDecl)
    }
  }

  def isSubTypeOf(superTypeDecl: TypeDecl, subTypeDecl: TypeDecl): Boolean = {
    if (superTypeDecl == subTypeDecl) {
      true
    } else if (subTypeDecl.name.lexeme == "Object") {
      false
    } else {
      val superClass = getSuperClass(subTypeDecl)
      val subTypeSuper = superClass.exists(isSubTypeOf(superTypeDecl, _))
      val interfaces = subTypeDecl.superInterfaces.foldRight(false) {
        case (interface, isSub) =>
          val interfaceDecl = environment.findExternType(interface, subTypeDecl).flatMap(environment.findType).getOrElse(throw Error.classNotFound(interface))
          isSubTypeOf(superTypeDecl, interfaceDecl) || isSub
      }
      subTypeSuper || interfaces
    }
  }

  def hasProtectedAccess(owner: FullyQualifiedID, ownerDecl: TypeDecl, accessorDecl: TypeDecl): Boolean = {
    if (samePackage(owner)) {
      true
    } else isSubTypeOf(ownerDecl, accessorDecl)
  }

  def samePackage(owner: FullyQualifiedID): Boolean = {
    environment.findQualifiedType(owner).exists(pack => pack.startsWith(environment.packageName))
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
        getSuperClass(typeDecl).flatMap {
          value =>
            if (typeDecl == value) {
              None
            } else {
              findField(id, value)
            }
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
    * @param id         Identifier of the method
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

        val superMethod = superClass flatMap {
          value =>
            if (typeDecl == value) {
              None
            } else {
              findMethod(id, parameters, value)
            }
        }

        superMethod match {
          case Some(value) => Some(value)
          case None =>
            val interfaces = typeDecl.superInterfaces.map(interfaceId =>
              environment.findType(interfaceId).getOrElse(throw Error.classNotFound(interfaceId)))
            interfaces.map(findMethod(id, parameters, _)).find(_.isDefined) match {
              case Some(value) => value
              case None => None
            }
        }
    }
  }

  protected def getSuperClass(typeDecl: TypeDecl): Option[TypeDecl] = {
    typeDecl.superClass match {
      case Some(value) =>
        environment.findExternType(value, typeDecl).flatMap(environment.findType) match {
          case Some(superClassDecl) => Some(superClassDecl)
          case None => throw Error.classNotFound(value.name)
        }
      case None =>
        environment.findType("Object") match {
          case Some(value) => Some(value)
          case None => throw Error.langLibraryNotLoaded
        }
    }
  }

  /**
    * Finds the static method based on the parameters types and the identifier.
    *
    * @param id         Identifier of the method
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

  /**
    * Finds non static method based on the parameters
    *
    * @param id         Identifier of the method
    * @param parameters The types of the parameters
    * @return The declaration of the method
    * @return
    */
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
    val (_, _, ctors) = partitionMembers(typeDecl.members)
    ctors.find {
      case ConstructorDecl(modifiers, _, parameterDecls, _) =>
        parametersMatch(parameters, parameterDecls)
      case _ => false
    }
  }

  /**
    * Disambiguates the name of id
    *
    * @param id         Ambiguous name
    * @param typeDecl   The declaration where it was found
    * @param scope      The scope currently where the name was found
    * @param parameters The parameters of the method where the name was found
    * @param isField    If the name is being called from a field
    * @param isStatic   If the name is being called from a static method
    * @return A Name denoting the kind of name and the type that was found
    */
  def findName(id: FullyQualifiedID, typeDecl: TypeDecl, scope: List[VarDecl],
               parameters: List[VarDecl], isField: Boolean, isStatic: Boolean): Name = {
    if (id.qualifiers.isEmpty) {
      if (isField && scope.exists(_.name.lexeme == id.id.lexeme)) {
        throw Error.identifierNotInScope(id)
      }
      val expr = (scope ++ parameters).find(_.name.lexeme == id.id.lexeme)
      expr match {
        case Some(value) =>
          ExprName(id, value.typ)
        case None =>
          findNonStaticField(id.id, typeDecl) match {
            case Some(value) =>
              if (isStatic) throw Error.cannotInvokeThisInStaticContext
              println(value._2.typ)
              ExprName(FullyQualifiedID(value._2.name), findFieldType(value, typeDecl, FullyQualifiedID(typeDecl.name)))
            case None =>
              environment.findType(id.id.lexeme) match {
                case Some(value) => TypeName(id, value)
                case None =>
                  if (environment.containsPackage(id.id.lexeme)) {
                    PackageName(id)
                  } else {
                    throw Error.noTopLevelPackage(id.name)
                  }
              }
          }
      }
    } else {
      findName(FullyQualifiedID(id.qualifiers), typeDecl, scope, parameters, isField, isStatic) match {
        case PackageName(packageId) =>
          if (environment.qualifiedTypes.contains(packageId.name)) {
            val types = environment.qualifiedTypes(packageId.name)
            val matchingTypes = types.filter(t => t.name.lexeme == id.id.lexeme)
            if (matchingTypes.lengthCompare(1) == 0) {
              TypeName(id, matchingTypes.head)
            } else {
              PackageName(id)
            }
          } else {
            PackageName(id)
          }
        case ExprName(exprId, typ) =>
          val field = id.id
          val exprType = typ match {
            case ArrayType(arrayOf, size) =>
              if (field.lexeme == "length") PrimitiveType(JavaInt(row = 0, col = 0))
              else throw Error.memberNotFound(s"$arrayOf[]", field)
            case NullType() => throw Error.nullPointerException
            case ClassType(typeID) =>
              val typeOf = environment.findType(typeID)
              typeOf.flatMap(findNonStaticField(field, _)) match {
                case Some(value) =>
                  findFieldType(value, typeDecl, typeID)
                case None => throw Error.classNotFound(typeID)
              }
            case PrimitiveType(typeToken) => throw Error.primitiveDoesNotContainField(typeToken, field)
          }
          ExprName(id, exprType)
        case TypeName(typeId, typeOf) =>
          findStaticField(id.id, typeOf) match {
            case Some(value) =>
              val fieldType = findFieldType(value, typeDecl, typeId)
              ExprName(id, fieldType)
            case None => throw Error.memberNotFound(typeId, id.id)
          }

        case AmbiguousName(ambiId) => throw Error.ambiguousName(ambiId)
      }
    }
  }

  /**
    * Changes a class to qualified type from a simple type based on the scope
    *
    * @param typeDecl The origin of the member that contains the original scope
    * @param typeOf   The simple or qualified type of the member
    * @return The fully qualified type of the member
    */
  def findMemberType(typeDecl: TypeDecl, typeOf: Type): Type = {
    typeOf match {
      case ClassType(externType) =>
        environment.findExternType(externType, typeDecl) match {
          case Some(fieldTypeId) => ClassType(FullyQualifiedID(fieldTypeId))
          case None => throw Error.classNotFound(externType)
        }
      case ArrayType(ClassType(externType), length) =>
        environment.findExternType(externType, typeDecl) match {
          case Some(fieldTypeId) => ArrayType(ClassType(FullyQualifiedID(fieldTypeId)), length)
          case None => throw Error.classNotFound(externType)
        }
      case NullType() => throw Error.nullPointerException
      case a: ArrayType => a
      case p: PrimitiveType => p
    }
  }

  /**
    * Finds the type of the field and checks if it has proper access
    *
    * @param value    The field declaration and type it was found in
    * @param typeDecl The type trying to access the field
    * @param typeId   The id of the type accessing the field
    * @return The type of the field fully qualified
    */
  def findFieldType(value: (TypeDecl, FieldDecl), typeDecl: TypeDecl, typeId: FullyQualifiedID): Type = {
    if (value._2.modifiers.exists(_.isInstanceOf[JavaProtected]) && !hasProtectedAccess(typeId, value._1, typeDecl)) {
      throw Error.protectedAccess(value._1, value._2.name)
    }
    findMemberType(value._1, value._2.typ)
  }

  /**
    * Finds the type of the method and checks for proper access
    *
    * @param value    The method declaration and type it was found in
    * @param typeDecl The type trying to access the field
    * @param typeId   The id of the type accessing the field
    * @return The type of the field fully qualified
    */
  def findMethodType(value: (TypeDecl, MethodDecl), typeDecl: TypeDecl, typeId: FullyQualifiedID): Option[Type] = {
    if (value._2.modifiers.exists(_.isInstanceOf[JavaProtected]) && !hasProtectedAccess(typeId, value._1, typeDecl)) {
      throw Error.protectedAccess(value._1, value._2.name)
    }
    value._2.returnType.map(findMemberType(value._1, _))
  }

}
