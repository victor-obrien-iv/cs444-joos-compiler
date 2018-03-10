package Disambiguator

import Environment._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

abstract class AugmentedVisitor {

  def visit(declAugmented: DeclAugmented): Any = declAugmented match {
    case CompilationUnitAugmented(fileName, packageName, imports, typeDecl, environment) =>
      println(fileName)
      visit(typeDecl)
    case ConstructorDeclAugmented(modifiers, identifier, parameters, body, environment) =>
      visit(body)
    case FieldDeclAugmented(modifiers, typ, name, assignment, environment) =>
      assignment map visit
    case MethodDeclAugmented(modifiers, returnType, name, parameters, body, environment) =>
      body map visit
    case ParameterDeclAugmented(typ, name, environment) =>
    case VarDeclAugmented(typ, name, environment) =>
    case _ =>
  }

  protected def visit(augmented: TypeDeclAugmented): Any = augmented match {
    case InterfaceDeclAugmented(modifiers, name, id, extensionOf, members, environment) =>
      members map visit
    case ClassDeclAugmented(modifiers, name, id, extensionOf, implementationOf, members, environment) =>
      members map visit
  }

  protected def visit(stmtAugmented: StmtAugmented): Any = stmtAugmented match {
    case BlockStmtAugmented(stmts, environment) => stmts map visit
    case DeclStmtAugmented(decl, assignment, environment) => assignment map visit
    case ExprStmtAugmented(expr, environment) => visit(expr)
    case ReturnStmtAugmented(expr, environment) => expr map visit
    case _ =>
  }

  protected def visit(exprAugmented: ExprAugmented): Any = exprAugmented match {
    case BinaryExprAugmented(lhs, operatorTok, rhs, environment) =>
      visit(lhs)
      visit(rhs)
    case UnaryExprAugmented(operatorTok, rhs, environment) => visit(rhs)
    case ParenExprAugmented(expr, environment) => visit(expr)
    case CallExprAugmented(obj, call, params, environment) =>
      obj map visit
      params map visit
    case ThisExprAugmented() =>
    case CastExprAugmented(castType, rhs, environment) => visit(rhs)
    case AccessExprAugmented(lhs, field, environment) => visit(lhs)
    case ArrayAccessExprAugmented(lhs, index, environment) => visit(lhs)
    case ValExprAugmented(value) =>
    case DeclRefExprAugmented(reference) =>
    case InstanceOfExprAugmented(lhs, typ, environment) => visit(lhs)
    case ObjNewExprAugmented(ctor, params, environment) => params map visit
    case ArrayNewExprAugmented(arrayType, environment) => arrayType.size map visit
    case NamedExprAugmented(name, environment) =>
  }

  def run(cu: CompilationUnitAugmented): Future[Any] = {
    Future(visit(cu))
  }
}
