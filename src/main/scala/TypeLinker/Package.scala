package TypeLinker

import AST.{FullyQualifiedID, TypeDecl}

case class Package(name: String, subPackages: List[Package], types: List[TypeDecl])
