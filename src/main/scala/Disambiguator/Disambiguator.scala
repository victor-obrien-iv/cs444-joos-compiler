package Disambiguator
import AST.{FieldDecl, FullyQualifiedID}
import Environment._
import Error.Error
import Token.JavaStatic

class Disambiguator extends AugmentedVisitor {

  override def visit(exprAugmented: ExprAugmented): Any = exprAugmented match {
    case NamedExprAugmented(name, environment) => findName(name, environment)
    case CallExprAugmented(obj, call, params, environment) =>
    case e => super.visit(e)
  }

  private def findName(id: FullyQualifiedID, environment: Environment): Name = {
    if (id.qualifiers.isEmpty) {
      if (id.id.lexeme == "i") {
        println(environment.variables.keys)
      }
      if (environment.variables.contains(id.id.lexeme)) {
        ExprName(id)
      } else if (environment.types.contains(id.id.lexeme)) {
        if (environment.types(id.id.lexeme).lengthCompare(1) != 0) {
          throw Error.multipleTypes(id)
        }
        TypeName(id, environment.types(id.id.lexeme).head)
      } else {
        if (environment.types.keys.exists(_.startsWith(id.id.lexeme))) {
          PackageName(id)
        } else {
          throw Error.noTopLevelPackage(id)
        }
      }
    } else {
      findName(FullyQualifiedID(id.qualifiers), environment) match {
        case PackageName(packageId) =>
          if (environment.types.contains(packageId.name)) {
            val types = environment.types(packageId.name)
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
          val fields = typeDecl.members.exists {
            case FieldDecl(modifiers, typ, name, assignment) =>
              if (modifiers.exists(_.isInstanceOf[JavaStatic])) {
                name.lexeme == id.id.lexeme
              } else {
                throw Error.nonStaticAccess(typeId, id.id)
              }
            case _ => false
          }
          if (fields) {
            ExprName(id)
          } else {
            throw Error.memberNotFound(typeId, id.id)
          }
        case AmbiguousName(ambiId) => throw Error.ambiguousName(ambiId)
      }
    }
  }
}
