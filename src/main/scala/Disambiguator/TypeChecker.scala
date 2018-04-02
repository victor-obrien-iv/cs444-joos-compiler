package Disambiguator

import AST._
import Environment._
import Error.Error
import Token._

class TypeChecker(val environment: Environment) extends EnvironmentBuilder(environment) {
  import scala.collection.mutable

  /**
    * cache what declaration an ast node refers to
    */
  val declCache: mutable.Map[AstNode, (TypeDecl, MemberDecl)] = mutable.Map()

  def build(compilationUnit: CompilationUnit): Unit = {
    val CompilationUnit(fileName, packageName, imports, typeDecl) = compilationUnit
    build(typeDecl, environment)
  }

  def build(typeDecl: TypeDecl, environment: Environment): Unit = typeDecl match {
    case InterfaceDecl(modifiers, name, id, extensionOf, members, packageName) =>
    case ClassDecl(modifiers, name, id, extensionOf, implementationOf, members, packageName) =>
      val (fields, methods, ctors) = partitionMembers(members)
      val (staticFields, nonStaticFields) = fields.partition(_.modifiers.exists(_.isInstanceOf[JavaStatic]))
      buildFields(staticFields, typeDecl, fields.map(field => VarDecl(field.typ, field.name)), isStatic = true)
      buildFields(fields, typeDecl, Nil, isStatic = false)
      methods.map{
        method =>
          val parameters = method.parameters.map(parameter => VarDecl(parameter.typ, parameter.name))
          method.body.map { (body: BlockStmt) =>
            build(body, typeDecl, Nil, parameters, method.returnType, method.modifiers.exists(_.isInstanceOf[JavaStatic]))
          }
      }
      ctors.map {
        ctor =>
          val superClass = getSuperClass(typeDecl).getOrElse(throw Error.classNotFound(typeDecl.name.lexeme))
          findConstructor(Nil, superClass) match {
            case Some(value) =>
            case None => throw Error.memberNotFound(superClass.name.lexeme, superClass.name)
          }
          val parameters = ctor.parameters.map(parameter => VarDecl(parameter.typ, parameter.name))
          build(ctor.body, typeDecl, Nil, parameters, None, isStatic = false)
      }
  }


  /**
    * Carries a list of forward references to check if a reference is called unexpectedly
    *
    * @param fields The list of fields being built
    * @param typeDecl The type the field is building
    */
  private def buildFields(fields: List[FieldDecl], typeDecl: TypeDecl, scope: List[VarDecl], isStatic: Boolean): Unit = {
    fields.foldRight(scope) {
      case (field, vars) =>
        val newScope = VarDecl(field.typ, field.name)::vars
        field.assignment.foreach{
          ass =>
            val typeAss = build(ass, typeDecl, newScope, Nil, isField = true, isStatic)
            if (!typeAssignable(field.typ, typeAss)) {
              throw Error.typeMismatch(typeAss, field.typ)
            }
        }
        newScope
    }
  }

  private def build(stmt: Stmt, typeDecl: TypeDecl, scope: List[VarDecl],
                    parameters: List[VarDecl], returnType: Option[Type], isStatic: Boolean): List[VarDecl] = stmt match {
    case BlockStmt(stmts) =>
      stmts.foldLeft(scope) {
      case (currentScope, currentStmt) =>
        build(currentStmt, typeDecl, currentScope, parameters, returnType, isStatic)
    }
    case DeclStmt(decl, assignment) =>
      val typeOf = assignment.map((expr: Expr) => build(expr, typeDecl, decl :: scope, parameters, isField =  false, isStatic))
      typeOf match {
        case Some(value) =>
          if (!typeAssignable(decl.typ, value))
            throw Error.typeMismatch(value, decl.typ)
        case None =>
      }
      decl :: scope
    case ExprStmt(expr) =>
      build(expr, typeDecl, scope, parameters, isField = false, isStatic)
      scope
    case ReturnStmt(expr) =>
      val exprType = expr.map((expr: Expr) => build(expr, typeDecl, scope, parameters, isField = false, isStatic))
      (returnType, exprType) match {
        case (Some(type1), Some(type2)) =>
          if (!typeAssignable(type1, type2)) {
            throw Error.typeMismatch(type2, type1)
          }
        case (None, None) =>
        case _ => throw Error.undefinedMatch
      }
      scope
    case c: CtrlFlowStmt => c match {
      case IfStmt(condition, thenStmt, elseStmt) =>
        val testType = build(condition, typeDecl, scope, parameters, isField = false, isStatic)
        tryBoolean(testType)
        build(thenStmt, typeDecl, scope, parameters, returnType, isStatic)
        elseStmt.map(build(_, typeDecl, scope, parameters, returnType, isStatic))
        scope
      case l: LoopStmt => l match {
        case ForStmt(init, condition, update, bodyStmt) =>
          val initDecl = init.map(build(_, typeDecl, scope, parameters, returnType, isStatic))
          val newScope = initDecl.getOrElse(scope)

          condition.map(build(_, typeDecl, newScope, parameters, isField = false, isStatic)) match {
            case Some(value) => tryBoolean(value)
            case None =>
          }

          update.map(build(_, typeDecl, newScope, parameters, returnType, isStatic))

          build(bodyStmt, typeDecl, newScope, parameters, returnType, isStatic)
        case WhileStmt(condition, bodyStmt) =>
          val condType = build(condition, typeDecl, scope, parameters, isField = false, isStatic)
          tryBoolean(condType)
          build(bodyStmt, typeDecl, scope, parameters, returnType, isStatic)
      }
    }
  }

  private def tryBoolean(testType: Type): Unit = {
    testType match {
      case PrimitiveType(_: JavaBoolean) =>
      case t => throw Error.expectedBoolean(t)
    }
  }


  /**
    * Builds the assignment expression of a field to check for forward references and static/non-static access
    *
    * @param expr The expression assigned to a field
    * @param typeDecl The type this field belongs to
    * @param scope The variables that it could potentially forward reference
    */
  protected def build(expr: Expr, typeDecl: TypeDecl, scope: List[VarDecl], parameters: List[VarDecl], isField: Boolean, isStatic: Boolean): Type = expr match {
    case BinaryExpr(lhs, _: Becomes, rhs) =>
      val leftType = build(lhs, typeDecl, scope, parameters, isField = false, isStatic)
      val rightType = build(rhs, typeDecl, scope, parameters, isField, isStatic)
      if (typeAssignable(leftType, rightType)) {
        leftType
      } else {
        throw Error.typeMismatch(rightType, leftType)
      }
    case BinaryExpr(lhs, operatorTok: Plus, rhs) =>
      val leftType = build(lhs, typeDecl, scope, parameters, isField, isStatic)
      val rightType = build(rhs, typeDecl, scope, parameters, isField, isStatic)
      (leftType, rightType) match {
        case (_, PrimitiveType(v: JavaVoid)) => throw Error.expectedNumeric(PrimitiveType(v))
        case (PrimitiveType(v: JavaVoid), _) => throw Error.expectedNumeric(PrimitiveType(v))
        case (ClassType(iD), _) if iD.name == "String" || iD.name == "java.lang.String" => ClassType(iD)
        case (_, ClassType(iD)) if iD.name == "String" || iD.name == "java.lang.String" => ClassType(iD)
        case (t1: PrimitiveType, t2: PrimitiveType) if t1.isNumeric && t2.isNumeric =>
          PrimitiveType(JavaInt(row = 0, col = 0))
        case (t1: PrimitiveType, t2) if t1.isNumeric => throw Error.expectedNumeric(t2)
        case (t, _) => throw Error.expectedNumeric(t)
      }
    case BinaryExpr(lhs, operatorTok: CompareOperator, rhs) =>
      val leftType = build(lhs, typeDecl, scope, parameters, isField, isStatic)
      val rightType = build(rhs, typeDecl, scope, parameters, isField, isStatic)
      (leftType, rightType) match {
        case (p1:PrimitiveType, p2:PrimitiveType) if p1.isNumeric && p2.isNumeric =>
          PrimitiveType(JavaBoolean(row = 0, col = 0))
        case (p1:PrimitiveType, t) if p1.isNumeric => throw Error.expectedNumeric(t)
        case (t, _) => throw Error.expectedBoolean(t)
      }
    case BinaryExpr(lhs, operatorTok: BooleanOperator, rhs) =>
      val leftType = build(lhs, typeDecl, scope, parameters, isField, isStatic)
      val rightType = build(rhs, typeDecl, scope, parameters, isField, isStatic)
      (leftType, rightType) match {
        case (PrimitiveType(_: JavaBoolean), PrimitiveType(_: JavaBoolean)) =>
          PrimitiveType(JavaBoolean(row = 0, col = 0))
        case (PrimitiveType(_:JavaBoolean), t) => throw Error.expectedBoolean(t)
        case (t, _) => throw Error.expectedBoolean(t)
      }
    case BinaryExpr(lhs, operatorTok: NumericOperator, rhs) =>
      val leftType = build(lhs, typeDecl, scope, parameters, isField, isStatic)
      val rightType = build(rhs, typeDecl, scope, parameters, isField, isStatic)
      (leftType, rightType) match {
        case (t1: PrimitiveType, t2: PrimitiveType) if t1.isNumeric && t2.isNumeric =>
          PrimitiveType(JavaInt(row = 0, col = 0))
        case (t1: PrimitiveType, t2) if t1.isNumeric => throw Error.expectedNumeric(t2)
        case (t, _) => throw Error.expectedNumeric(t)
      }
    case BinaryExpr(lhs, operatorTok: EqualityOperator, rhs) =>
      val leftType = build(lhs, typeDecl, scope, parameters, isField, isStatic)
      val rightType = build(rhs, typeDecl, scope, parameters, isField, isStatic)
      if (typeAssignable(leftType, rightType) || typeAssignable(rightType, leftType)) {
        PrimitiveType(JavaBoolean(row = 0, col = 0))
      } else {
        throw Error.typeMismatch(leftType, rightType)
      }
    case UnaryExpr(operatorTok: Bang, rhs) =>
      build(rhs, typeDecl, scope, parameters, isField, isStatic) match {
        case PrimitiveType(b: JavaBoolean) => PrimitiveType(b)
        case t => throw Error.expectedBoolean(t)
      }
    case UnaryExpr(operatorTok: Minus, rhs) =>
      build(rhs, typeDecl, scope, parameters, isField, isStatic) match {
        case p: PrimitiveType if p.isNumeric => p
        case t => throw Error.expectedNumeric(t)
      }
    case ParenExpr(parenExpr) => build(parenExpr, typeDecl, scope, parameters, isField, isStatic)
    case CallExpr(obj, call, params) =>
      var isClass = false //flag to consider finding a static member or nah//
      var typeId = FullyQualifiedID("java.lang.Object")
      val objTypeDecl = obj.flatMap{ (expr: Expr) =>
        build(expr, typeDecl, scope, parameters, isField, isStatic) match {
          case ClassType(id) =>
            typeId = id
            environment.findType(id)
          case Class(id) =>
            typeId = id
            isClass = true
            environment.findType(id)
          case _ : ArrayType =>
            environment.findType("java.lang.Object")
          case PrimitiveType(primitive) => throw Error.accessPrimitiveType(primitive, call)
          case NullType() => throw Error.nullPointerException
        }
      }
      val paramTypes = params.map(build(_, typeDecl, scope, parameters, isField, isStatic))
      val methodType = objTypeDecl match {
        case Some(value) =>
          if (isClass)
            findStaticMethod(call, paramTypes, value)
          else findNonStaticMethod(call, paramTypes, value)
        case None =>
          if (isClass)
            throw Error.memberNotFound(typeDecl.name.lexeme, call)
          else if (isStatic)
            throw Error.cannotInvokeThisInStaticContext
          else findNonStaticMethod(call, paramTypes, typeDecl)

      }
      val returnType = methodType match {
        case Some(value) =>
          declCache(expr) = value
          findMethodType(value, typeDecl, typeId)
        case None =>
          throw Error.memberNotFound(objTypeDecl.map(_.name).toString, call)
      }
      returnType match {
        case Some(value) => value
        case None => PrimitiveType(JavaVoid(row = 0, col = 0))
      }
    case ThisExpr() =>
      if (isStatic) throw Error.cannotInvokeThisInStaticContext
      environment.findQualifiedType(typeDecl.name.lexeme) match {
        case Some(value) => ClassType(FullyQualifiedID(value))
        case None => throw Error.classNotFound(typeDecl.name.lexeme)
      }
    case CastExpr(castType, rhs) =>
      val rightType = build(rhs, typeDecl, scope, parameters, isField, isStatic)
      (castType, rightType) match {
        case (p1: PrimitiveType, p2: PrimitiveType) if p1.isNumeric && p2.isNumeric =>
          castType
        case (t1, t2) if typeAssignable(t2, t1) || typeAssignable(t1, t2) =>
          castType
        case _ => throw Error.typeMismatch(rightType, castType)
      }
    case AccessExpr(lhs, field) =>
      build(lhs, typeDecl, scope, parameters, isField, isStatic) match {
        case ArrayType(arrayOf, size) =>
          if (field.lexeme == "length") PrimitiveType(JavaInt(row = 0, col = 0))
          else throw Error.memberNotFound(s"$arrayOf[]", field)
        case NullType() => throw Error.nullPointerException
        case ClassType(typeID) =>
          val typeOf = environment.findType(typeID)
          println(typeOf.map(_.members.map(_.name)))
          typeOf.flatMap(findNonStaticField(field, _)) match {
            case Some(value) =>
              findFieldType(value, typeDecl, typeID)
            case None => throw Error.classNotFound(typeID)
          }
        case PrimitiveType(typeToken) => throw Error.primitiveDoesNotContainField(typeToken, field)
      }
    case ArrayAccessExpr(lhs, index) =>
      println(lhs)
      val arrayType = build(lhs, typeDecl, scope, parameters, isField, isStatic) match {
        case a: ArrayType => a
        case e => throw Error.notArray(e)
      }
      build(index, typeDecl, scope, parameters, isField, isStatic) match {
        case p: PrimitiveType if p.isNumeric =>
        case r => throw Error.typeMismatch(r, PrimitiveType(JavaInt(row = 0, col = 0)))
      }
      arrayType.arrayOf

    case ValExpr(value) =>
      value match {
        case IntegerLiteral(_, row, col, _) => PrimitiveType(JavaInt(row = row, col = col))
        case BooleanLiteral(row, col, _) => PrimitiveType(JavaBoolean(row = row, col = col))
        case CharacterLiteral(_, row, col, _) => PrimitiveType(JavaChar(row = row, col = col))
        case _:StringLiteral => ClassType(FullyQualifiedID("java.lang.String"))
        case _:NullLiteral => NullType()
      }
    case DeclRefExpr(reference) =>
      findName(FullyQualifiedID(reference), typeDecl, scope, parameters, isField, isStatic) match {
        case ExprName(id, typ: Type, decls) =>
          decls foreach { d => declCache(expr) = d }
          typ
        case _ => throw Error.classNotFound(reference.lexeme)
      }
    case InstanceOfExpr(lhs, typ) =>
      val typeOfLValue = build(lhs, typeDecl, scope, parameters, isField, isStatic)
      if (typeAssignable(typeOfLValue, typ) || typeAssignable(typ, typeOfLValue)) {
        PrimitiveType(JavaBoolean(row = 0, col = 0))
      } else {
        throw Error.typeMismatch(typeOfLValue, typ)
      }
    case e: NewExpr => e match {
      case ObjNewExpr(ctor, params) =>
        val newType = environment.findType(ctor).getOrElse(throw Error.classNotFound(ctor))
        if (newType.modifiers.exists(_.isInstanceOf[JavaAbstract])) {
          throw Error.cannotInstantiateAbstract(newType)
        }
        val paramTypes = params.map((expr: Expr) => build(expr, typeDecl, scope, parameters, isField, isStatic))
        findConstructor(paramTypes, newType) match {
          case Some(constructorDecl) =>
            declCache(e) = (newType, constructorDecl)
            if (constructorDecl.modifiers.exists(_.isInstanceOf[JavaProtected]) && !samePackage(ctor)) {
              throw Error.protectedAccess(newType, ctor.id)
            }
            ClassType(ctor)
          case None => throw Error.classNotFound(ctor)
        }
      case ArrayNewExpr(arrayType) =>
        arrayType.size.foreach {
          expr =>
            build(expr, typeDecl, scope, parameters, isField, isStatic) match {
              case p: PrimitiveType if p.isNumeric =>
              case r => throw Error.typeMismatch(r, PrimitiveType(JavaInt(row = 0, col = 0)))
            }
        }
        arrayType
    }

    case NamedExpr(name) =>
      findName(name, typeDecl, scope, parameters, isField, isStatic) match {
        case ExprName(id, typ, decls) =>
          decls foreach { d => declCache(expr) = d }
          typ
        case TypeName(id, typ) =>
          environment.findQualifiedType(id.name) match {
            case Some(qualifiedType) => Class(FullyQualifiedID(qualifiedType))
            case None => throw Error.classNotFound(id)
        }
        case _ =>
          throw Error.classNotFound(name)
      }
  }

}
