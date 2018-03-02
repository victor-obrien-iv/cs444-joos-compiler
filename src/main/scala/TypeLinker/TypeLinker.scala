package TypeLinker

import AST.{CompilationUnit, FullyQualifiedID, ImportDecl, TypeDecl}
import Token.Identifier

class TypeLinker {

  def buildContext(units: List[CompilationUnit]): Map[Option[FullyQualifiedID], List[TypeDecl]] = {
    val ctx = units.groupBy(_.packageName).mapValues(_.map(_.typeDecl))
    val classNames = ctx.flatMap {
      case (packageId, typeDecls) =>
        typeDecls.map {
          decl =>
            packageId match {
              case Some(id) =>
                s"${id.name}.${decl.name.lexeme}"
              case None =>
            }
        }
    }
    classNames foreach {
      name => ctx foreach  {
        case (packageName, _) =>
          packageName match {
            case Some(value) =>
              if (value.name.contains(name + ".") || value.name == name) {
                throw Error.Error(value.name,
                  s"package ${value.name} conflicts with class $name", Error.Type.TypeLinking)
              }
            case None =>
          }
      }
    }
    println(classNames)
    println(ctx.keys.map(_.map(_.name)))
    ctx
  }

//  def buildLocalContext(unit: CompilationUnit,
//                        typeCtx: Map[Option[FullyQualifiedID], List[TypeDecl]]): Map[Identifier, TypeDecl] = {
//    val containsLang = unit.imports.exists {
//      importDecl =>
//        importDecl.name.name == "java.lang" && importDecl.asterisk
//    }
//    unit.imports map {
//      importDecl =>
//
//    }
//  }

}