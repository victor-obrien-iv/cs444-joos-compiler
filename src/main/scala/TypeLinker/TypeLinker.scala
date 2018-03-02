package TypeLinker

import AST.{CompilationUnit, TypeDecl}

class TypeLinker {

  def buildContext(units: List[CompilationUnit]): Map[String, List[TypeDecl]] = {
    val ctx = units.groupBy(_.packageName.map(_.name).getOrElse("")).mapValues(_.map(_.typeDecl))
    val classNames = ctx.flatMap {
      case (packageId, typeDecls) =>
        typeDecls.map {
          decl => s"$packageId.${decl.name.lexeme}"
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

    ctx
  }

  def buildLocalContext(unit: CompilationUnit,
                        typeCtx: Map[String, List[TypeDecl]]): List[TypeDecl] = {
    val containsLang = unit.imports.exists {
      importDecl =>
        importDecl.name.name == "java.lang" && importDecl.asterisk
    }
    unit.imports flatMap {
      importDecl =>
        if (importDecl.asterisk) {
          val packageName = importDecl.name.name
          println(packageName)
          val allPackages = typeCtx.filterKeys(p => p.startsWith(packageName + ".") || p == packageName)
          if (allPackages.nonEmpty) {
            allPackages.flatMap(_._2)
          } else {
            throw Error.Error(packageName, s"Could not find package $packageName", Error.Type.TypeLinking)
          }
        } else {
          val packageName = importDecl.name.qualifiers.map(_.lexeme).mkString(".")
          if (typeCtx.contains(packageName)) {
              val classDecl = typeCtx(packageName).find(_.name.lexeme == importDecl.name.id.lexeme)
              classDecl match {
                case Some(classType) => List(classType)
                case None => throw Error.Error(importDecl.name.id.lexeme, s"Could not find import ${importDecl.name.name}",
                  Error.Type.TypeLinking)
              }
          } else {
            throw Error.Error(packageName, s"Could not find package $packageName", Error.Type.TypeLinking)
          }
        }
    }
  }

}