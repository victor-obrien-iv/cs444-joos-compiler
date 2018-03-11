package Environment

import AST._
import Token.JavaStatic
import TypeLinker.TypeContextBuilder

import scala.collection.{immutable, mutable}

class EnvironmentBuilder {

  def build(unit: CompilationUnit, environment: Environment): CompilationUnitAugmented = {
    val typeAugmented = build(unit.typeDecl, environment)

    CompilationUnitAugmented(unit.fileName, unit.packageName,
      unit.imports, typeAugmented, environment)
  }

  private def build(decl: TypeDecl, environment: Environment): TypeDeclAugmented = decl match {
    case c: ClassDecl => build(c, environment)
    case i: InterfaceDecl => build(i, environment)
  }

  private def build(classDecl: ClassDecl, environment: Environment): ClassDeclAugmented = {

    val (fields, methods, constructors) = (
      classDecl.members.collect{case field: FieldDecl => field},
      classDecl.members.collect{case method: MethodDecl => method},
      classDecl.members.collect{case ctor: ConstructorDecl => ctor}
    )

    val (staticFields, nonStaticFields) = fields.partition {
      field =>
        field.modifiers.exists(_.isInstanceOf[JavaStatic])
    }

    val staticFieldsAugmented = build(staticFields, environment)
    val staticEnv = staticFieldsAugmented match {
      case Nil => environment
      case _ => staticFieldsAugmented.last.environment
    }

    val staticEnvWithThis = staticEnv.copy(types = staticEnv.types + ("this" -> List(classDecl)))
    val nonStaticFieldsAugmented = build(nonStaticFields, staticEnv)

    val fieldsAugmented = staticFieldsAugmented ++ nonStaticFieldsAugmented

    val fieldBindings = fieldsAugmented.map {
      field =>
        (field.name.lexeme, (field.typ, field.assignment))
    }

    val methodBindings: List[(MethodHeader, Option[BlockStmt])] =
      methods.map {
        case MethodDecl(modifiers, returnType, name, parameters, body) =>
          val returnTypeAugmented = returnType.map(build(_, Environment.empty))
          val parameterDeclAugmented = parameters.map(build(_, Environment.empty))
          (MethodHeader(modifiers, returnTypeAugmented, name, parameterDeclAugmented), body)
      }

    val methodNames = methodBindings.map {
      case (header, _) => (header, None)
    }

    val constructorSplit = constructors.map {
      case ConstructorDecl(modifiers, identifier, parameters, body) =>
        val parameterDeclAugmented = parameters.map(build(_, Environment.empty))
        (ConstructorHeader(modifiers, identifier, parameterDeclAugmented), body)
    }

    val constructorParams: List[(ConstructorHeader, BlockStmtAugmented)] = constructorSplit.map {
      case (header, _) => (header, BlockStmtAugmented(Nil, Environment.empty))
    }

    val fromTypesEnvironment = Environment(
      environment.types,
      fieldBindings.toMap,
      mutable.Map(methodNames: _*),
      mutable.Map(constructorParams: _*)
    )

    val methodsAugmented = methodBindings.map {
      case (header, body) =>
        val params = header.parameters.map {
          parameter => (parameter.name.lexeme, (parameter.typ, None))
        }
        val newVars = fromTypesEnvironment.variables ++ params.toMap
        val newEnvironment = fromTypesEnvironment.copy(variables = newVars)
        val bodyAugmented = body.map(build(_, newEnvironment))
        fromTypesEnvironment.methods(header) = bodyAugmented
        MethodDeclAugmented(header.modifiers, header.returnType,
          header.name, header.parameters, bodyAugmented, newEnvironment)
    }

    val constructorAugmented = constructorSplit.map {
      case (header, body) =>
        val params = header.parameters.map {
          parameter => (parameter.name.lexeme, (parameter.typ, None))
        }
        val newVars = fromTypesEnvironment.variables ++ params.toMap
        val newEnvironment = fromTypesEnvironment.copy(variables = newVars)
        val bodyAugmented = build(body, newEnvironment)
        fromTypesEnvironment.constructors(header) = bodyAugmented
        ConstructorDeclAugmented(header.modifiers, header.identifier, header.parameters, bodyAugmented, newEnvironment)
    }

    val members = fieldsAugmented ++ methodsAugmented ++ constructorAugmented

    ClassDeclAugmented(classDecl.modifiers, classDecl.name, classDecl.id,
      classDecl.extensionOf, classDecl.implementationOf, members, fromTypesEnvironment)
  }

  private def build(staticFields: List[FieldDecl], environment: Environment): List[FieldDeclAugmented] = {
    staticFields.foldLeft(List.empty[FieldDeclAugmented]) {
      case (list, field) =>
        val oldEnv = list match {
          case Nil => environment
          case _ => list.last.environment
        }
        val exprAugmented = field.assignment map {
          build(_, oldEnv)
        }
        val newType = build(field.typ, oldEnv)
        val binding = field.name.lexeme -> (newType, exprAugmented)
        val newEnv = environment.copy(variables = oldEnv.variables + binding)
        val fieldAugmented = FieldDeclAugmented(field.modifiers, newType, field.name, exprAugmented, newEnv)
        list :+ fieldAugmented
    }
  }

  private def build(decl: InterfaceDecl, environment: Environment): InterfaceDeclAugmented = {
    val methods: List[(MethodHeader, Option[BlockStmtAugmented])] =
      decl.members.map {
        case MethodDecl(modifiers, returnType, name, parameters, _) =>
          val returnTypeAugmented = returnType.map(build(_, Environment.empty))
          val parameterDeclAugmented = parameters.map(build(_, Environment.empty))
          (MethodHeader(modifiers, returnTypeAugmented, name, parameterDeclAugmented), None)
      }

    val environment = Environment(Map.empty, Map.empty, mutable.Map(methods: _*), mutable.Map.empty)

    val methodsAugmented = methods.map {
      case (header, body) =>
        MethodDeclAugmented(header.modifiers, header.returnType, header.name, header.parameters, body, environment)
    }

    InterfaceDeclAugmented(decl.modifiers, decl.name, decl.id, decl.extensionOf, methodsAugmented, environment)
  }

  private def build(parameterDecl: ParameterDecl, environment: Environment): ParameterDeclAugmented = {
    val typeAugmented = build(parameterDecl.typ, environment)
    ParameterDeclAugmented(typeAugmented, parameterDecl.name, environment)
  }

  private def build(blockStmt: BlockStmt, environment: Environment): BlockStmtAugmented = {
    val seqStmts = blockStmt.stmts.foldLeft(List.empty[StmtAugmented]) {
      case (Nil, stmt) =>
        List(build(stmt, environment))
      case (head :: tail, stmt) =>
        build(stmt, head.environment) :: head :: tail
    }
    BlockStmtAugmented(seqStmts, environment)
  }

  private def build(stmt: Stmt, environment: Environment): StmtAugmented = stmt match {
    case bs: BlockStmt => build(bs, environment)
    case DeclStmt(decl, assignment) =>
      val declAugmented = build(decl, environment)
      val assignmentAugmented = assignment.map(build(_, environment))
      val newBindings = environment.variables + (decl.name.lexeme -> (declAugmented.typ, assignmentAugmented))
      val newEnvironment = environment.copy(variables = newBindings)
      DeclStmtAugmented(declAugmented, assignmentAugmented, newEnvironment)
    case ExprStmt(expr) => ExprStmtAugmented(build(expr, environment), environment)
    case ReturnStmt(expr) => ReturnStmtAugmented(expr.map(build(_, environment)), environment)
    case IfStmt(condition, thenStmt, elseStmt) =>
      val conditionAugmented = build(condition, environment)
      val thenStmtAugmented = build(thenStmt, environment)
      val elseStmtAugmented = elseStmt.map(build(_, environment))
      IfStmtAugmented(conditionAugmented, thenStmtAugmented, elseStmtAugmented, environment)
    case WhileStmt(condition, bodyStmt) =>
      val conditionAugmented = build(condition, environment)
      val bodyStmtAugmented = build(bodyStmt, environment)
      WhileStmtAugmented(conditionAugmented, bodyStmtAugmented, environment)
    case ForStmt(init, condition, update, bodyStmt) =>
      val initAugmented = init.map(build(_, environment))
      val newEnvironment = initAugmented match {
        case Some(value) => value.environment
        case None => environment
      }
      val conditionAugmented = condition.map(build(_, newEnvironment))
      val updateAugmented = update.map(build(_, newEnvironment))
      val stmtAugmented = build(bodyStmt, newEnvironment)

      ForStmtAugmented(initAugmented, conditionAugmented, updateAugmented, stmtAugmented, environment)
  }

  private def build(decl: VarDecl, environment: Environment): VarDeclAugmented = decl match {
    case VarDecl(typ, name) =>
      val typeAugmented = build(typ, environment)
      VarDeclAugmented(typeAugmented, name, environment)
  }

  private def build(expr: Expr, environment: Environment): ExprAugmented = expr match {
    case BinaryExpr(lhs, operatorTok, rhs) =>
      val lhsAug = build(lhs, environment)
      val rhsAug = build(rhs, environment)
      BinaryExprAugmented(lhsAug, operatorTok, rhsAug, environment)
    case UnaryExpr(operatorTok, rhs) =>
      val rhsAug = build(rhs, environment)
      UnaryExprAugmented(operatorTok, rhsAug, environment)
    case ParenExpr(innerExpr) =>
      val exprAug = build(innerExpr, environment)
      ParenExprAugmented(exprAug, environment)
    case CallExpr(obj, call, params) =>
      val objAug = obj.map(build(_, environment))
      val paramsAug = params.map(build(_, environment))
      CallExprAugmented(objAug, call, paramsAug, environment)
    case ThisExpr() =>
      ThisExprAugmented()
    case CastExpr(castType, rhs) =>
      val rhsAug = build(rhs, environment)
      val castTypeAug = build(castType, environment)
      CastExprAugmented(castTypeAug, rhsAug, environment)
    case AccessExpr(lhs, field) =>
      val lhsAug = build(lhs, environment)
      AccessExprAugmented(lhsAug, field, environment)
    case ArrayAccessExpr(lhs, index) =>
      val lhsAug = build(lhs, environment)
      val indexAug = build(index, environment)
      ArrayAccessExprAugmented(lhsAug, indexAug, environment)
    case ValExpr(value) => ValExprAugmented(value)
    case DeclRefExpr(reference) => DeclRefExprAugmented(reference)
    case InstanceOfExpr(lhs, typ) =>
      val lhsAug = build(lhs, environment)
      val typeAug = build(typ, environment)
      InstanceOfExprAugmented(lhsAug, typeAug, environment)
    case ObjNewExpr(ctor, params) =>
      val paramsAug = params.map(build(_, environment))
      ObjNewExprAugmented(ctor, paramsAug, environment)
    case ArrayNewExpr(arrayType) =>
      val arrayTypeAugmented = build(arrayType, environment).asInstanceOf[ArrayTypeAugmented]
      ArrayNewExprAugmented(arrayTypeAugmented, environment)
    case NamedExpr(name) => NamedExprAugmented(name, environment)
  }

  private def build(typ: Type, environment: Environment): TypeAugmented = typ match {
    case ArrayType(arrayOf, size) =>
      val expr = size.map(build(_, environment))
      val typ = build(arrayOf, environment)
      ArrayTypeAugmented(typ, expr, environment)
    case PrimitiveType(typeToken) => PrimitiveTypeAugmented(typeToken)
    case ClassType(typeID) => ClassTypeAugmented(typeID)
  }

}
