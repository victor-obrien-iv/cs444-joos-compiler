package TypeLinker

import AST.{CompilationUnit, FullyQualifiedID, ImportDecl, TypeDecl}
import Token.Identifier

class TypeLinker {

  def buildContext(units: List[CompilationUnit]): List[Package] = {
    val ctx = units.groupBy(_.packageName.map(_.name).getOrElse("")).mapValues(_.map(_.typeDecl))
    val classNames = ctx.flatMap {
      case (packageId, typeDecls) =>
        typeDecls.map {
          decl => s"$packageId.${decl.name.lexeme}"
        }
    }

    val packages = ctx.map{
      case (packageName, classes) =>
        Package(packageName, Nil, classes)
    }

    val sortedCtx = packages.toSeq.sortBy(_.name.split(".").length).foldLeft(List.empty[Package]) {
      (listSoFar: List[Package], curPackage: Package) =>
        if (curPackage.name == "") {
          curPackage::listSoFar
        } else {
          Package(curPackage.name, listSoFar.filter(_.name.contains(curPackage.name + ".")), curPackage.types) :: listSoFar
        }
    }

    classNames foreach {
      name => ctx foreach  {
        case (packageName, _) =>
          if (packageName.contains(name + ".") || packageName == name) {
                throw Error.Error(packageName,
                  s"package $packageName conflicts with class $name", Error.Type.TypeLinking)
              }
          }
      }

    sortedCtx
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