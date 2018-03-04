package TypeLinker

import AST.{CompilationUnit, FullyQualifiedID, ImportDecl, TypeDecl}
import Token.Identifier


class TypeContextBuilder {

  /**
    * Provides a reference of all qualified types throughout all provided compilation units
    *
    * @param units All units found in the system
    * @return A map of the package name -> all types and their ASTs found in that package
    */
  def buildContext(units: List[CompilationUnit]): Map[String, List[TypeDecl]] = {
    val ctx = units.groupBy(_.packageName.map(_.name).getOrElse("")).mapValues(_.map(_.typeDecl))
    val classNames = ctx.flatMap {
      case (packageId, typeDecls) =>
        typeDecls.map {
          decl => s"$packageId.${decl.name.lexeme}"
        }
    }

    classNames foreach {
      name =>
        ctx foreach {
          case (packageName, _) =>
            if (packageName.contains(name + ".") || packageName == name) {
              throw Error.Error(packageName,
                s"package $packageName conflicts with class $name", Error.Type.TypeLinking)
            }
        }
    }

    ctx
  }

  /**
    * Provides a reference of simple identifers for types to their TypeDecl
    *
    * @param unit The CompilationUnit that we are building a context for
    * @param typeCtx The full Qualified Type context
    * @return An association list of simple types -> the type AST
    */
  def buildLocalContext(unit: CompilationUnit,
                        typeCtx: Map[String, List[TypeDecl]]): Map[String, List[TypeDecl]] = {

    val defaultPackage = unit.packageName match {
      case Some(value) => value.name
      case None => ""
    }

    val wildCards = unit.imports.filter(_.asterisk)
    val onDemandImports = defaultPackage :: "java.lang" :: wildCards.map(_.name.name)
    val onDemand = onDemandImports flatMap {
      packageName =>
        val allPackages = typeCtx.filterKeys(p => p.startsWith(packageName + ".") || p == packageName)
        if (allPackages.nonEmpty) allPackages.flatMap {
          case (key, value) =>
            value.map { typeDecl =>
              val mapKey = if (packageName != "") key + "." + typeDecl.name.lexeme else typeDecl.name.lexeme
              (mapKey, typeDecl)
          }
        } else {
          throw Error.Error(packageName, s"Could not find package $packageName", Error.Type.TypeLinking)
        }
    }

    val declaredTypes = unit.imports.filterNot(_.asterisk).map {
      importDecl =>
        val packageName = importDecl.name.qualifiers.map(_.lexeme).mkString(".")
        if (typeCtx.contains(packageName)) {
          val classDecl = typeCtx(packageName).find(_.name.lexeme == importDecl.name.id.lexeme)
          classDecl match {
            case Some(classType) =>
              val mapKey = if (packageName != "") packageName + "." + classType.name.lexeme else classType.name.lexeme
              (mapKey, classType)
            case None => throw Error.Error(importDecl.name.id.lexeme, s"Could not find import ${importDecl.name.name}",
              Error.Type.TypeLinking)
          }
        } else {
          throw Error.Error(packageName, s"Could not find package $packageName", Error.Type.TypeLinking)
        }
    }

    //Eliminates duplicate entries in list
    val onDemandUniqueListTypes = onDemand.toMap.toList

    val onDemandListTypes = onDemandUniqueListTypes.groupBy {
      fullName =>
        if (fullName._1.isEmpty) {
          ""
        } else {
          fullName._1.split('.').last
        }
    }

    val singleImportTypes = declaredTypes.groupBy(_._1.split('.').last)

    val allTypes = onDemandListTypes ++ singleImportTypes

    val duplicates  = singleImportTypes.mapValues(_.groupBy(_._1))

    if (duplicateTypes(declaredTypes)) {
      throw Error.Error("duplicate", s"Type clash error", Error.Type.TypeLinking)
    }

    val typeNames = allTypes map {
      case (name, decls) => (name.split('.').last, decls.map(_._2))
    }

    typeNames
  }

  /**
    * Finds if there are duplicate types with different package names
    *
    * @param list A List of package name + type name -> type declaration
    * @return Whether there are duplicates with different package names
    */
  private def duplicateTypes(list: List[(String, TypeDecl)]): Boolean = {
    val duplicates = list.groupBy(_._1.split('.').last).mapValues(_.groupBy(_._1))
    duplicates.exists(_._2.size > 1)
  }

}