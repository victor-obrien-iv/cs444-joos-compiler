package TypeLinker

import AST.{CompilationUnit, FullyQualifiedID, TypeDecl}

class TypeLinker {

  def buildContext(units: List[CompilationUnit]): Map[Option[FullyQualifiedID], List[TypeDecl]] = {
    units.groupBy(_.packageName).mapValues(_.map(_.typeDecl))
  }

}
