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

  def buildLocalContext(unit: CompilationUnit,
                        typeCtx: List[Package]): List[TypeDecl] = {
    val containsLang = unit.imports.exists {
      importDecl =>
        importDecl.name.name == "java.lang" && importDecl.asterisk
    }
    typeCtx.foreach(p => println(p.name))
    println
    unit.imports flatMap {
      importDecl =>
        if (importDecl.asterisk) {
          val packageName = importDecl.name.name
          println(packageName)
          val packageGroup = typeCtx.find(_.name == packageName)
          packageGroup match {
            case Some(value) => typesFromPackage(value)
            case None => throw Error.Error(packageName, s"Could not find package $packageName", Error.Type.TypeLinking)
          }
        } else {
          val packageName = importDecl.name.qualifiers.mkString(".")
          println(packageName)
          val packageGroup = typeCtx.find(_.name == packageName)
          packageGroup match {
            case Some(value) =>
              val classDecl = value.types.find(_.name.lexeme == importDecl.name.id.lexeme)
              classDecl match {
                case Some(classType) => List(classType)
                case None => throw Error.Error(importDecl.name.id.lexeme, s"Could not find import ${importDecl.name}",
                  Error.Type.TypeLinking)
              }
            case None => throw Error.Error(packageName, s"Could not find package $packageName", Error.Type.TypeLinking)
          }
        }
    }
  }

  private def typesFromPackage(packagedClasses: Package): List[TypeDecl] = {
    packagedClasses.types ++ packagedClasses.subPackages.flatMap(p => p.types ++ typesFromPackage(p))
  }

}