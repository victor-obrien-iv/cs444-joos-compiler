package TypeLinker

import AST.{CompilationUnit, FullyQualifiedID, ImportDecl, TypeDecl}
import Token.Identifier

import scala.collection.immutable

class TypeContextBuilder {

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

  def buildLocalContext(unit: CompilationUnit,
                        typeCtx: Map[String, List[TypeDecl]]): Map[String, TypeDecl]= {

    val javaLangIdentifier = FullyQualifiedID(List(Identifier("java",0,0)), Identifier("lang",0,0))
    val javaLangImport = ImportDecl(javaLangIdentifier, asterisk = true)

    val packageQualifiers = unit.packageName match {
      case Some(FullyQualifiedID(qualifiers, id)) => qualifiers :+ id
      case None => Nil
    }

    val curClassImport = ImportDecl(FullyQualifiedID(packageQualifiers, unit.typeDecl.name), asterisk = false)
    val imports = unit.imports /*:+ javaLangImport*/ :+ curClassImport

    val wildCards = imports.filter(_.asterisk) flatMap {
      importDecl =>
        val packageName = importDecl.name.name
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

    val declaredTypes = imports.filterNot(_.asterisk).map {
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


    val allTypes = wildCards ++ declaredTypes

    val duplicates  = declaredTypes.groupBy(_._1.split('.').last).mapValues(_.groupBy(_._1))
    if (duplicates.exists(_._2.size > 1)) {
      throw Error.Error("duplicate", s"Type clash error", Error.Type.TypeLinking)
    }

    allTypes.toMap
  }

}