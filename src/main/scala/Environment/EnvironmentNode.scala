package Environment

import AST.{AstNode, Decl, MethodDecl}

case class EnvironmentNode(ast: AstNode, variables: List[(String, Decl)], methods: List[(String, List[MethodDecl])])