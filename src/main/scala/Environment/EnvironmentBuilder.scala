package Environment

import AST._

import scala.collection.mutable

class EnvironmentBuilder {

  def build(unit: CompilationUnit): CompilationUnitAugmented = {
    CompilationUnitAugmented(unit.fileName, unit.packageName, unit.imports, build(unit.typeDecl))
  }

  private def build(decl: TypeDecl): TypeDeclAugmented = {
    case c: ClassDecl => build(c)
    case i: InterfaceDecl => build(i)
  }

  private def build(classDecl: ClassDecl): ClassDeclAugmented = {

    val fieldsAugmented = classDecl.members.filter(_.isInstanceOf[FieldDecl]).map {
      case f: FieldDecl =>
        val expr = f.assignment.map(build(_, Environment.empty))
        val typ = build(f.typ, Environment.empty)
        ((typ, f.name.lexeme), FieldDeclAugmented(f.modifiers, typ, f.name, expr, Environment.empty))
    }

    val decls = fieldsAugmented.map {
      case (key, value) => (key, value.assignment)
    }

    val methods: List[(MethodHeader, Option[BlockStmt])] =
      classDecl.members.filter(_.isInstanceOf[MethodDecl]).map {
        case MethodDecl(modifiers, returnType, name, parameters, body) =>
          val returnTypeAugmented = returnType.map(build(_, Environment.empty))
          val parameterDeclAugmented = parameters.map(build(_, Environment.empty))
          (MethodHeader(modifiers, returnTypeAugmented, name, parameterDeclAugmented), body)
      }

    val methodNames = methods.map {
      case (header, _) => (header, None)
    }

    val constructors = classDecl.members.filter(_.isInstanceOf[ConstructorDecl]).map {
      case ConstructorDecl(modifiers, identifier, parameters, body) =>
        val parameterDeclAugmented = parameters.map(build(_, Environment.empty))
        (ConstructorHeader(modifiers, identifier, parameterDeclAugmented), body)
    }

    val constructorParams: List[(ConstructorHeader, BlockStmtAugmented)] = constructors.map {
      case (header, _) => (header, BlockStmtAugmented(Nil, Environment.empty))
    }

    val environment = Environment(decls, mutable.Map(methodNames: _*), mutable.Map(constructorParams: _*))

    val methodsAugmented = methods.map {
      case (header, body) =>
        val params = header.parameters.map {
          parameter => ((parameter.typ, parameter.name.lexeme), None)
        }
        val newVars = params ++ environment.variables
        val newEnvironment = environment.copy(variables = newVars)
        val bodyAugmented = body.map(build(_, newEnvironment))
        environment.methods(header) = bodyAugmented
        MethodDeclAugmented(header.modifiers, header.returnType,
          header.name, header.parameters, bodyAugmented, newEnvironment)
    }

    val constructorAugmented = constructors.map {
      case (header, body) =>
        val bodyAugmented = build(body, environment)
        environment.constructors(header) = bodyAugmented
        ConstructorDeclAugmented(header.modifiers, header.identifier, header.parameters, bodyAugmented, environment)
    }

    val members = fieldsAugmented.map(_._2) ++ methodsAugmented

    ClassDeclAugmented(classDecl.modifiers, classDecl.name, classDecl.id,
      classDecl.extensionOf, classDecl.implementationOf, members, environment)
  }

  private def build(decl: InterfaceDecl): InterfaceDeclAugmented = {
    val methods: List[(MethodHeader, Option[BlockStmtAugmented])] =
      decl.members.map {
        case MethodDecl(modifiers, returnType, name, parameters, body) =>
          val returnTypeAugmented = returnType.map(build(_, Environment.empty))
          val parameterDeclAugmented = parameters.map(build(_, Environment.empty))
          (MethodHeader(modifiers, returnTypeAugmented, name, parameterDeclAugmented), None)
      }

    val environment = Environment(Nil, mutable.Map(methods: _*), mutable.Map.empty)

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

  private def build(methodDecl: MethodDecl, environment: Environment): MethodDeclAugmented = {
    case MethodDecl(modifiers, returnType, name, parameters, body) =>
      val typeAug = returnType.map(build(_, Environment.empty))
      val parameterDeclAugmented = parameters.map {
        case ParameterDecl(typ, paramName) =>
          val parameterTypeAug = build(typ, Environment.empty)
          ParameterDeclAugmented(parameterTypeAug, paramName, Environment.empty)
      }
      val bodyAugmented = body.map(build(_, Environment.empty))
      MethodDeclAugmented(modifiers, typeAug, name, parameterDeclAugmented, bodyAugmented, Environment.empty)
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

  private def build(stmt: Stmt, environment: Environment): StmtAugmented = {
    case bs: BlockStmt => build(bs, environment)
    case DeclStmt(decl, assignment) =>
      val declAugmented = build(decl, environment)
      val assignmentAugmented = assignment.map(build(_, environment))
      val newBindings = ((declAugmented.typ, decl.name.lexeme), assignmentAugmented) :: environment.variables
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

  private def build(decl: VarDecl, environment: Environment): VarDeclAugmented = {
    case VarDecl(typ, name) =>
      val typeAugmented = build(typ, environment)
      VarDeclAugmented(typeAugmented, name, environment)
  }

  private def build(expr: Expr, environment: Environment): ExprAugmented = {
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
      ThisExprAugmented
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
    case ObjNewExpr(ctor, params) =>
      val paramsAug = params.map(build(_, environment))
      ObjNewExprAugmented(ctor, paramsAug, environment)
    case ArrayNewExpr(arrayType) =>
      val arrayTypeAugmented = build(arrayType, environment).asInstanceOf[ArrayTypeAugmented]
      ArrayNewExprAugmented(arrayTypeAugmented, environment)
    case NamedExpr(name) => NamedExprAugmented(name, environment)
  }

  private def build(typ: Type, environment: Environment): TypeAugmented = {
    case ArrayType(arrayOf, size) =>
      val expr = size.map(build(_, environment))
      val typ = build(arrayOf, environment)
      ArrayTypeAugmented(typ, expr, environment)
    case PrimitiveType(typeToken) => PrimitiveTypeAugmented(typeToken)
    case ClassType(typeID) => ClassTypeAugmented(typeID)
  }

}
