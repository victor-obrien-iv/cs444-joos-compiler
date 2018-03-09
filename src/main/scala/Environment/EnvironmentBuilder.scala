package Environment

import AST._

import scala.collection.mutable

class EnvironmentBuilder {

  /**
    * Provides a reference of all qualified types throughout all provided compilation units
    *
    * @param units All units found in the system
    * @return A map of the package name -> all types and their ASTs found in that package
    */
  def buildContext(units: List[CompilationUnit]): mutable.Map[String, Option[TypeDeclAugmented]] = {
    val map = units.groupBy {
      unit => unit.packageName match {
        case Some(value) => s"$value.${unit.typeDecl.name.lexeme}"
        case None => unit.typeDecl.name.lexeme
      }
    }.mapValues(_ => None)
    mutable.Map(map.toSeq: _*)
  }

  private def buildLocalContextNames(unit: CompilationUnit,
                                     environment: Environment): mutable.Map[String, Option[TypeDeclAugmented]] = {
    val className = unit.typeDecl.name.lexeme

    val defaultPackage = unit.packageName match {
      case Some(value) => value.name
      case None => ""
    }

    val typeCtx = environment.types

    val fullClassName = s"$defaultPackage.$className"

    val wildCards = unit.imports.filter(_.asterisk)
    val onDemandImports = "java.lang" :: wildCards.map(_.name.name)

    /**
      * Creates a map of package names all of their member types
      *
      * @param packageName Root package
      * @return
      */
    def packageMemberClasses(packageName: String): Map[String, Option[TypeDeclAugmented]] = {
      typeCtx.filterKeys(p => p.startsWith(packageName + ".") || p == packageName).toMap
    }

    val onDemand = onDemandImports flatMap packageMemberClasses

    val declaredTypes = unit.imports.filterNot(_.asterisk).map {
      importDecl =>
        val qualifiedID = importDecl.name
        (qualifiedID.id.lexeme, typeCtx(qualifiedID.name))
    }

    val uniqueSingleImportTypes = declaredTypes.toMap

    val singleImportTypes: Map[String, Option[TypeDeclAugmented]] = if (uniqueSingleImportTypes.contains(fullClassName)) {
      uniqueSingleImportTypes - fullClassName
    } else {
      uniqueSingleImportTypes
    }

    //Wraps default package in the same format as OnDemand and SingleType
    val defaultPackageClasses: Map[String, Option[TypeDeclAugmented]] = packageMemberClasses(defaultPackage)

    //Order matters: updates facilitates shadowing, so each map will overwrite bindings to the left
    val allTypes = onDemand.toMap ++ defaultPackageClasses ++ singleImportTypes
    mutable.Map(allTypes.mapValues(_ => None).toSeq: _*)
  }

  /**
    * Provides a reference of simple identifers for types to their TypeDecl
    *
    * @param unit The CompilationUnit that we are building a context for
    * @return An association list of simple types -> the type AST
    */
  def buildLocalContext(unit: CompilationUnitAugmented): Unit = {

    val className = unit.typeDecl.name.lexeme

    val defaultPackage = unit.packageName match {
      case Some(value) => value.name
      case None => ""
    }

    val environment = unit.environment
    val typeCtx = environment.types

    val fullClassName = s"$defaultPackage.$className"

    val wildCards = unit.imports.filter(_.asterisk)
    val onDemandImports = "java.lang" :: wildCards.map(_.name.name)

    /**
      * Creates a map of package names all of their member types
      *
      * @param packageName Root package
      * @return
      */
    def packageMemberClasses(packageName: String): Map[String, Option[TypeDeclAugmented]] = {
      typeCtx.filterKeys(p => p.startsWith(packageName + ".") || p == packageName).toMap
    }

    val onDemand = onDemandImports flatMap packageMemberClasses

    val declaredTypes = unit.imports.filterNot(_.asterisk).map {
      importDecl =>
        val qualifiedID = importDecl.name
        (qualifiedID.id.lexeme, typeCtx(qualifiedID.name))
    }

    val uniqueSingleImportTypes = declaredTypes.toMap

    val singleImportTypes: Map[String, Option[TypeDeclAugmented]] = if (uniqueSingleImportTypes.contains(fullClassName)) {
      uniqueSingleImportTypes - fullClassName
    } else {
      uniqueSingleImportTypes
    }

    //Wraps default package in the same format as OnDemand and SingleType
    val defaultPackageClasses: Map[String, Option[TypeDeclAugmented]] = packageMemberClasses(defaultPackage)

    //Order matters: updates facilitates shadowing, so each map will overwrite bindings to the left
    val allTypes = onDemand.toMap ++ defaultPackageClasses ++ singleImportTypes

    allTypes foreach {
      case (key, value) =>
        environment.types(key) = value
    }
  }

  def build(unit: CompilationUnit, environment: Environment): CompilationUnitAugmented = {
    val localContext = buildLocalContextNames(unit, environment)
    val newEnvironment = environment.copy(types = environment.types ++ localContext)
    CompilationUnitAugmented(unit.fileName, unit.packageName,
      unit.imports, build(unit.typeDecl, newEnvironment), newEnvironment)
  }

  private def build(decl: TypeDecl, environment: Environment): TypeDeclAugmented = decl match {
    case c: ClassDecl => build(c, environment)
    case i: InterfaceDecl => build(i, environment)
  }

  private def build(classDecl: ClassDecl, environment: Environment): ClassDeclAugmented = {

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

    val environment = Environment(mutable.Map.empty, decls, mutable.Map(methodNames: _*), mutable.Map(constructorParams: _*))

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

    val members = fieldsAugmented.map(_._2) ++ methodsAugmented ++ constructorAugmented

    ClassDeclAugmented(classDecl.modifiers, classDecl.name, classDecl.id,
      classDecl.extensionOf, classDecl.implementationOf, members, environment)
  }

  private def build(decl: InterfaceDecl, environment: Environment): InterfaceDeclAugmented = {
    val methods: List[(MethodHeader, Option[BlockStmtAugmented])] =
      decl.members.map {
        case MethodDecl(modifiers, returnType, name, parameters, _) =>
          val returnTypeAugmented = returnType.map(build(_, Environment.empty))
          val parameterDeclAugmented = parameters.map(build(_, Environment.empty))
          (MethodHeader(modifiers, returnTypeAugmented, name, parameterDeclAugmented), None)
      }

    val environment = Environment(mutable.Map.empty, Nil, mutable.Map(methods: _*), mutable.Map.empty)

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
