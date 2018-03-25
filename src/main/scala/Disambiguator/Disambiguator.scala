package Disambiguator
import AST._
import Environment._
import Error.Error
import Token.{Becomes, JavaStatic}

class Disambiguator(environment: Environment) extends EnvironmentBuilder[Unit](environment) {

  override def build(typeDecl: TypeDecl, environment: Environment): Unit = typeDecl match {
    case InterfaceDecl(modifiers, name, id, extensionOf, members) =>
    case ClassDecl(modifiers, name, id, extensionOf, implementationOf, members) =>
      val (fields, methods, ctors) = partitionMembers(members)
      val (staticFields, nonStaticFields) = fields.partition(_.modifiers.exists(_.isInstanceOf[JavaStatic]))
  }

  private def partitionMembers(decls: List[MemberDecl]): (List[FieldDecl], List[MethodDecl], List[ConstructorDecl]) =
    decls.foldRight((List.empty[FieldDecl], List.empty[MethodDecl], List.empty[ConstructorDecl])) {
      case (fieldDecl: FieldDecl, (fields, methods, ctors)) => (fieldDecl :: fields, methods, ctors)
      case (methodDecl: MethodDecl, (fields, methods, ctors)) => (fields, methodDecl :: methods, ctors)
      case (ctorDecl: ConstructorDecl, (fields, methods, ctors)) => (fields, methods, ctorDecl :: ctors)
  }

  private def buildFields(fields: List[FieldDecl], typeDecl: TypeDecl, scope: List[VarDecl]): List[VarDecl] = {
    fields.foldLeft(scope) {
      case (vars, field) =>
        field.assignment.map(build(_, typeDecl, vars))
        VarDecl(field.typ, field.name)::vars
    }
  }

  private def buildField(expr: Expr, typeDecl: TypeDecl, scope: List[VarDecl]): Unit = expr match {
    case BinaryExpr(lhs, operatorTok, rhs) if operatorTok.isInstanceOf[Becomes] =>
      val (fields, _, _) = partitionMembers(typeDecl.members)
      build(lhs, )
      build(rhs, typeDecl, scope)
    case _ => build(expr, typeDecl, scope)
  }

  protected override def build(expr: Expr, typeDecl: TypeDecl, scope: List[VarDecl]): Environment = expr match {
    case BinaryExpr(lhs, operatorTok, rhs) =>
    case UnaryExpr(operatorTok, rhs) =>
    case ParenExpr(expr) =>
    case CallExpr(obj, call, params) =>
    case ThisExpr() =>
    case CastExpr(castType, rhs) =>
    case AccessExpr(lhs, field) =>
    case ArrayAccessExpr(lhs, index) =>
    case ValExpr(value) =>
    case DeclRefExpr(reference) =>
    case InstanceOfExpr(lhs, typ) =>
    case _: NewExpr =>
    case NamedExpr(name) =>
  }


  private def findName(id: FullyQualifiedID, environment: Environment): Name = {
    if (id.qualifiers.isEmpty) {
      if (environment.variables.contains(id.id.lexeme)) {
        ExprName(id)
      } else if (environment.staticFields.contains(id.id.lexeme)) {
        ExprName(id)
      } else if (environment.types.contains(id.id.lexeme)) {
        if (environment.types(id.id.lexeme).lengthCompare(1) != 0) {
          throw Error.multipleTypes(id)
        }
        TypeName(id, environment.types(id.id.lexeme).head)
      } else {
        if (environment.qualifiedTypes.keys.exists(_.startsWith(id.id.lexeme))) {
          PackageName(id)
        } else {
          throw Error.noTopLevelPackage(id)
        }
      }
    } else {
      findName(FullyQualifiedID(id.qualifiers), environment) match {
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
        case ExprName(exprId) =>
          ExprName(exprId)
        case TypeName(typeId, typeDecl) =>
          val typeEnv = build(typeDecl, environment)

          if (typeEnv.staticFields.contains(id.id.lexeme)) {
            ExprName(id)
          } else {
            throw Error.memberNotFound(typeId, id.id)
          }

        case AmbiguousName(ambiId) => throw Error.ambiguousName(ambiId)
      }
    }
  }
}
