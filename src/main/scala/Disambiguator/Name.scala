package Disambiguator

import AST.{FullyQualifiedID, Type, TypeDecl}

sealed trait Name {
  def id: FullyQualifiedID
}

case class PackageName(id: FullyQualifiedID) extends Name

case class ExprName(id: FullyQualifiedID, typ: Type) extends Name

case class TypeName(id: FullyQualifiedID, typeDecl: TypeDecl) extends Name

case class AmbiguousName(id: FullyQualifiedID) extends Name