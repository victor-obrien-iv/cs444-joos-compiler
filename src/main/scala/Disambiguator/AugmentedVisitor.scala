package Disambiguator

import AST._
import Environment._
import Token.JavaStatic

abstract class AugmentedVisitor {

  def visit(decl: Decl, environment: Environment): Environment = decl match {
    case CompilationUnit(fileName, packageName, imports, typeDecl) =>
      val typeEnv = visit(typeDecl, environment)
      typeDecl.members map (visit(_, typeEnv))
      typeEnv

    case ConstructorDecl(modifiers, identifier, parameters, body) =>
      val header = ConstructorHeader(identifier, parameters)
      val newConstructors = environment.constructors + (header -> body)
      environment.copy(constructors = newConstructors)

    case FieldDecl(modifiers, typ, name, assignment) =>
//      assignment.map(visit(_, environment))
      val newVariables = environment.variables + (name.lexeme -> (typ, assignment))
      if (modifiers.exists(_.isInstanceOf[JavaStatic])) {
        environment.copy(staticVars = newVariables)
      } else {
        environment.copy(variables = newVariables)
      }

    case MethodDecl(modifiers, returnType, name, parameters, body) =>
      val header = MethodHeader(name, parameters)
      val parametersEnv = parameters.foldLeft(environment) {
        case (oldEnv, parameter) =>
          visit(parameter, oldEnv)
      }
      val bodyEnv = body.map(visit(_, parametersEnv))
      val newMethods = environment.methods + (header -> (returnType, body))
      environment.copy(methods = newMethods)

    case ParameterDecl(typ, name) =>
      val newVariables = environment.variables + (name.lexeme -> (typ, None))
      environment.copy(variables = newVariables)

    case VarDecl(typ, name) => environment
    case _ => environment
  }

  protected def visit(typeDecl: TypeDecl, environment: Environment): Environment = typeDecl match {
    case InterfaceDecl(modifiers, name, id, extensionOf, members) =>
      val superClassEnv = extensionOf.foldLeft(environment) {
        case (superEnvs, interface) =>
          visit(superEnvs.types(interface.name).head, superEnvs)
      }

      val methods = typeDecl.members map {
        case MethodDecl(_, returnType, methodName, parameters, body) =>
          MethodHeader(methodName, parameters) -> (returnType, body)
      }

      environment.copy(methods = methods.toMap)

    case ClassDecl(modifiers, name, id, extensionOf, implementationOf, members) =>
      val superClassEnv = if (name.lexeme == "Object") environment else {
        val superClass = extensionOf match {
          case Some(value) => environment.findType(value)
          case None => environment.findType("Object")
        }
        visit(superClass, environment)
      }

      visit(members, typeDecl, superClassEnv)
  }

  protected def visit(members: List[MemberDecl], classDecl: TypeDecl, environment: Environment): Environment = {
    val (fields, methods) = members.partition(_.isInstanceOf[FieldDecl])
    val (staticFields, nonStaticFields) = fields.partition(_.modifiers.exists(_.isInstanceOf[JavaStatic]))

    val methodEnv = visitMethods(methods, environment)

    val staticEnv = visit(staticFields, methodEnv)
    visit(nonStaticFields, staticEnv)
  }

  protected def visit(decls: List[Decl], environment: Environment): Environment = {
    decls.foldLeft(environment) {
      case (oldEnv, decl) =>
        visit(decl, oldEnv)
    }
  }

  protected def visitMethods(methodDecls: List[MemberDecl], environment: Environment): Environment = {
    val (memberMethods, constructorDecls) = methodDecls.partition(_.isInstanceOf[MethodDecl])
    val methodMap = memberMethods map {
      case MethodDecl(_, returnType, methodName, parameters, body) =>
        MethodHeader(methodName, parameters) -> (returnType, body)
    }
    
    val constructorMap = constructorDecls map {
      case ConstructorDecl(modifiers, className, parameters, body) =>
        ConstructorHeader(className, parameters) -> body
    }

    environment.copy(methods = methodMap.toMap, constructors = constructorMap.toMap)
  }

  protected def visit(stmt: Stmt, environment: Environment): Environment = stmt match {
    case BlockStmt(stmts) => stmts.foldLeft(environment) {
      case (oldEnv, nextStmt) => visit(nextStmt, oldEnv)
    }
    case DeclStmt(decl, assignment) =>
      val newVariables = environment.variables + (decl.name.lexeme -> (decl.typ, assignment))
      environment.copy(variables = newVariables)
    case ExprStmt(expr) => visit(expr, environment)
    case ReturnStmt(expr) => expr.map(visit(_, environment)).getOrElse(environment)
    case _ => environment
  }

  protected def visit(typeOf: Type, environment: Environment): Environment = typeOf match {
    case ArrayType(arrayOf, size) =>
      visit(arrayOf, environment)
      size.map(visit(_, environment)).getOrElse(environment)
    case PrimitiveType(typeToken) => environment
    case ClassType(typeID) => visit(environment.types(typeID.name).head, environment)
  }

  protected def visit(expr: Expr, environment: Environment): Environment = expr match {
    case BinaryExpr(lhs, operatorTok, rhs) =>
      visit(lhs, environment)
      visit(rhs, environment)
    case UnaryExpr(operatorTok, rhs) =>
      visit(rhs, environment)
    case ParenExpr(innerExpr) =>
      visit(innerExpr, environment)
    case CallExpr(obj, call, params) =>
      obj.map(visit(_, environment)).getOrElse(environment)
    case ThisExpr() => environment
    case CastExpr(castType, rhs) => visit(rhs, environment)
    case AccessExpr(lhs, field) => visit(lhs, environment)
    case ArrayAccessExpr(lhs, index) =>
      visit(lhs, environment)
      visit(index, environment)
    case ValExpr(value) => environment
    case DeclRefExpr(reference) => visit(environment.types(reference.lexeme).head, environment)
    case InstanceOfExpr(lhs, typ) =>
      visit(lhs, environment)
      visit(typ, environment)
    case ObjNewExpr(ctor, params) => environment
    case ArrayNewExpr(arrayType) =>
      visit(arrayType, environment)
    case NamedExpr(name) => environment
  }

}
