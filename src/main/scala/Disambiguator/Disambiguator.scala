package Disambiguator
import AST._
import Environment._
import Error.Error
import Token.{Becomes, JavaStatic}

class Disambiguator extends AugmentedVisitor {

  protected override def visit(expr: Expr, environment: Environment): Environment = expr match {
    case BinaryExpr(lhs, operatorTok, rhs) if operatorTok.isInstanceOf[Becomes] =>
      visit(lhs, environment.copy(variables = environment.variables ++ environment.allFields))
      visit(rhs, environment)
    case NamedExpr(name) =>
      findName(name, environment)
      environment
    case CallExpr(obj, call, params) => environment
    case e => super.visit(e, environment)
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
          val typeEnv = visit(typeDecl, environment)

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
