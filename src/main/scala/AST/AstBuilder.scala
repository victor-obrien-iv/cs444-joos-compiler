package AST

import Error._
import Parser.TreeNode
import Token._

class AstBuilder(filename: String) {

  private def AstError(token: Token) = {
    Error(token.lexeme,
      "Unexpected token",
      Type.ASTBuilder,
      Some(Location(token.row, token.col, filename)))
  }

  private def AstError(node: TreeNode) = {
    Error(node.state.toString,
      "Unexpected Nonterminal",
      Type.ASTBuilder,
      None
    )
  }

  def build(node: TreeNode): CompilationUnit = {
    buildCompilationUnit(node)
  }

  private def buildCompilationUnit(node: TreeNode): CompilationUnit = {

    val packageDeclaration = buildPackageAst(node.children(1))
    val imports = buildImports(node.children(2))
    val typeDeclaration = buildTypeDecl(node.children(3), packageDeclaration)

    CompilationUnit(filename, packageDeclaration, imports, typeDeclaration)
  }

  private def buildPackageAst(node: TreeNode): Option[FullyQualifiedID] = {
    if (node.children.nonEmpty)
      Some(buildFullyQualifiedId(node.children(1)))
    else None
  }

  private def buildFullyQualifiedId(node: TreeNode): FullyQualifiedID = {
    val idNode = if (node.children.lengthCompare(1) == 0) node.children.head else node.children(2)
    val id = idNode.state.left.get.asInstanceOf[Identifier]

    if (node.children.lengthCompare(1) == 0)
      FullyQualifiedID(Nil, id)
    else
      FullyQualifiedID(buildQualifiers(node.children.head), id)
  }

  private def buildQualifiers(node: TreeNode): List[Identifier] = {
    val idNode = if (node.children.lengthCompare(1) == 0) node.children.head else node.children(2)
    val id = idNode.state.left.get.asInstanceOf[Identifier]

    if (node.children.lengthCompare(1) == 0) {
      List(id)
    } else {
      buildQualifiers(node.children.head) :+ id
    }
  }

  private def buildImports(node: TreeNode): List[ImportDecl] = {
    if (node.children.isEmpty) {
      Nil
    } else {
      buildImports(node.children.head) :+ buildImport(node.children(1))
    }
  }

  private def buildImport(node: TreeNode): ImportDecl = {
    val id = buildFullyQualifiedId(node.children(1))
    if (node.children.lengthCompare(3) == 0) ImportDecl(id, asterisk = false) else ImportDecl(id, asterisk = true)
  }

  def buildTypeDecl(node: TreeNode, packageName: Option[FullyQualifiedID]): TypeDecl = {
    val typeDeclaration = node.children.head
    val modifiers = buildModifiers(typeDeclaration.children.head)

    val actualDeclaration = typeDeclaration.children(1)
    actualDeclaration.state match {
      case Right("InterfaceDeclaration")  => buildInterfaceDeclaration(modifiers, actualDeclaration, packageName)
      case Right("ClassDeclaration") => buildClassDeclaration(modifiers, actualDeclaration, packageName)
      case Right(_) => throw AstError(actualDeclaration)
      case Left(token) => throw AstError(token)
    }
  }


  private def buildModifiers(node: TreeNode): List[Modifier] = {
//    println(node)
    if (node.children.lengthCompare(1) == 0) {
      List(node.children.head.children.head.state.left.get.asInstanceOf[Modifier])
    } else {
//      println(node.children(1).children.head)
      buildModifiers(node.children.head) :+ node.children(1).children.head.state.left.get.asInstanceOf[Modifier]
    }
  }

  private def buildInterfaceDeclaration(modifiers: List[Modifier], node: TreeNode, packageName: Option[FullyQualifiedID]): InterfaceDecl = {
    val identifier = node.children(1).state.left.get.asInstanceOf[Identifier]
    val superInterfaces = if (node.children.lengthCompare(3) == 0) Nil else buildInterfaceList(node.children(3))
//    println(node.state)
//    println(node.children.map(node => node.state))
    val body = if (node.children.lengthCompare(3) == 0) node.children(2) else node.children(4)
    val bodyDecls = buildInterfaceBody(body.children(1))

    InterfaceDecl(modifiers, identifier, filename.hashCode, superInterfaces, bodyDecls, packageName)
  }

  private def buildInterfaceBody(node: TreeNode): List[MemberDecl] = {
    if (node.children.nonEmpty) {
      //InterfaceBodyDeclarations InterfaceBodyDeclarations InterfaceBodyDeclaration
      if (node.children(1).children.lengthCompare(2) == 0) {
        //InterfaceBodyDeclaration public InterfaceMethodDeclaration
        buildInterfaceBody(node.children.head) :+ buildInterfaceBodyDeclaration(node.children(1))
      } else {
        //InterfaceBodyDeclaration ;
        buildInterfaceBody(node.children.head)
      }
    } else {
      //InterfaceBodyDeclarations
      Nil
    }
  }

  private def buildInterfaceBodyDeclaration(node: TreeNode): MethodDecl = {
    //InterfaceBodyDeclaration InterfaceModifiers InterfaceMethodDeclaration
    val modifiers = buildModifiers(node.children.head)
    val methodDecl = node.children(1)

    //InterfaceMethodDeclaration MethodHeader ;
    val (typ, identifier, parameters) = buildMethodHeader(methodDecl.children.head)

    MethodDecl(modifiers, typ, identifier, parameters, None)
  }

  private def buildClassDeclaration(modifiers: List[Modifier], node: TreeNode, packageName: Option[FullyQualifiedID]): ClassDecl = {
    val id = node.children(1).state.left.get.asInstanceOf[Identifier]
    val superCLass = buildSuperClass(node.children(2))
    val superInterfaces = buildSuperInterfaces(node.children(3))
    val body = buildClassBody(node.children(4).children(1))

    ClassDecl(modifiers, id, filename.hashCode, superCLass, superInterfaces, body, packageName)
  }

  private def buildSuperClass(node: TreeNode): Option[FullyQualifiedID] = {
    if (node.children.nonEmpty) {
      Some(buildFullyQualifiedId(node.children(1)))
    } else {
      None
    }
  }

  private def buildSuperInterfaces(node: TreeNode): List[FullyQualifiedID] = {
    if (node.children.nonEmpty) {
      buildInterfaceList(node.children(1))
    } else {
      Nil
    }
  }

  private def buildInterfaceList(node: TreeNode): List[FullyQualifiedID] = {
//    println(node)
    if (node.children.lengthCompare(1) == 0) {
      List(buildFullyQualifiedId(node.children.head))
    } else {
      buildInterfaceList(node.children.head) :+ buildFullyQualifiedId(node.children(2))
    }
  }

  private def buildClassBody(node: TreeNode): List[MemberDecl] = {
    if (node.children.nonEmpty) {
      buildClassBody(node.children.head) :+ buildBodyDeclaration(node.children(1))
    } else {
      Nil
    }
  }

  private def buildBodyDeclaration(node: TreeNode): MemberDecl = {
    val head = node.children.head
    head match {
      case TreeNode(Right("ConstructorDeclaration"), _) =>
        buildConstructorDecl(head)
      case TreeNode(Right("ClassMemberDeclaration"), children) =>
        children.head.state match {
          case Right("MethodDeclaration") =>
            buildMethodDecl(children.head)
          case Right("FieldDeclaration") =>
            buildFieldDecl(children.head)
          case Right(_) => throw AstError(node)
          case Left(token) => throw AstError(token)
        }
      case TreeNode(Left(token), _) => throw AstError(head)
      case _ => throw AstError(head)
    }
  }

  private def buildConstructorDecl(node: TreeNode): ConstructorDecl = {
    val modifiers = buildModifiers(node.children.head)
    val identifier = node.children(1).state.left.get.asInstanceOf[Identifier]
    val parameters = buildFormalParameters(node.children(2))
    val body = buildMethodBody(node.children(3)).getOrElse(throw AstError(node))

    ConstructorDecl(modifiers, identifier, parameters, body)
  }

  private def buildMethodDecl(node: TreeNode): MethodDecl = {
    val modifiers = buildModifiers(node.children.head)
    if (node.children.lengthCompare(3) == 0) {
      val (typ, identifier, parameters) = buildMethodHeader(node.children(1))
      val body = buildMethodBody(node.children(2))

      MethodDecl(modifiers, typ, identifier, parameters, body)
    } else {
      val nativeModifiers = modifiers ++ (node.children(1).state.left.get.asInstanceOf[Modifier]
        :: node.children(2).state.left.get.asInstanceOf[Modifier] :: Nil)
      val typ = PrimitiveType(node.children(3).state.left.get.asInstanceOf[JavaInt])
      val id = node.children(4).state.left.get.asInstanceOf[Identifier]
      val parameterDecl = ParameterDecl(buildType(node.children(6)), node.children(7).state.left.get.asInstanceOf[Identifier])
      val body = buildMethodBody(node.children(9))

      MethodDecl(nativeModifiers, Some(typ), id, List(parameterDecl), body)
    }
  }

  private def buildFieldDecl(node: TreeNode): FieldDecl = {
    val modifiers = buildModifiers(node.children.head)
    val typ = buildType(node.children(1))
    val varDecl = node.children(2)

    val identifier = varDecl.children.head.state.left.get.asInstanceOf[Identifier]
    val assignment = if (varDecl.children.lengthCompare(3) == 0) {
      Some(buildExpr(varDecl.children(2).children.head))
    } else None

    FieldDecl(modifiers, typ, identifier, assignment)
  }

  private def buildExpr(node: TreeNode): Expr = {
    node match {
      case TreeNode(Left(value), _) if value.isInstanceOf[Identifier] =>
        DeclRefExpr(value.asInstanceOf[Identifier])
      case TreeNode(Right("FieldAccess"), children) =>
        buildFieldAccess(node)
      case TreeNode(Right("ArrayAccess"), children) =>
        buildArrayAccess(node)
      case TreeNode(Right("Primary"), _) =>
        buildPrimary(node)
      case TreeNode(Right("PrimaryNoNewArray"), children) =>
        buildNoNewArrayPrimary(node)
      case TreeNode(Right("MethodInvocation"), _) =>
        buildMethodInvocation(node)
      case TreeNode(Right("ClassInstanceCreationExpression"), children) =>
        ObjNewExpr(buildFullyQualifiedId(children(1)), buildArguments(children(2)))
      case TreeNode(Right("Name"), _) =>
        NamedExpr(buildFullyQualifiedId(node))
      case TreeNode(_, children) =>
        children.length match {
          case 1 => buildExpr(children.head)
          case 2 => UnaryExpr(children.head.state.left.get.asInstanceOf[UnaryOperator], buildExpr(node.children(1)))
          case 3 =>
            //Casts the token as operator if it is an operator, else the operator is only one level deeper
            // for all grammar rules of binary operators
            val operator = if (children(1).state.isLeft) {
              children(1).state.left.get.asInstanceOf[Operator]
            } else {
              children(1).children.head.state.left.get.asInstanceOf[Operator]
            }
            operator match {
              case io: JavaInstanceof => InstanceOfExpr(buildExpr(children.head), buildType(children(2)))
              case _ => BinaryExpr(buildExpr(children.head), operator.asInstanceOf[BinaryOperator], buildExpr(children(2)))
            }

          case 4 =>
            CastExpr(buildCastType(node.children(1)), buildExpr(node.children(3)))
          case _ => throw AstError(node)
        }
    }
  }

  private def buildCastType(node: TreeNode): Type = node.state match {
    case Right(value) =>
      value match {
        case "Name" | "PrimitiveType" | "ReferenceType" | "ArrayType" => buildType(node)
        case _ if node.children.lengthCompare(1) == 0 => buildCastType(node.children.head)
        case _ => throw AstError(node)
      }
    case Left(token) => throw AstError(token)
  }

  private def buildFieldAccess(node: TreeNode): AccessExpr = {
    AccessExpr(buildPrimary(node.children.head), node.children(2).state.left.get.asInstanceOf[Identifier])
  }

  private def buildArrayAccess(node: TreeNode): ArrayAccessExpr = {
    ArrayAccessExpr(buildExpr(node.children.head), buildExpr(node.children(2)))
  }

  private def buildPrimary(node: TreeNode): Expr = node.children.head match {
    case TreeNode(Right("PrimaryNoNewArray"), children) => buildNoNewArrayPrimary(node.children.head)
    case TreeNode(Right("ArrayCreationExpression"), children) => buildArrayCreationExpression(node.children.head)
    case _ => throw AstError(node)
  }

  private def buildNoNewArrayPrimary(node: TreeNode): Expr = node.children.head match {
    case TreeNode(Right(value), children) =>
      value match {
        case "Literal" => ValExpr(children.head.state.left.get.asInstanceOf[Literal])
        case "ClassInstanceCreationExpression" => ObjNewExpr(buildFullyQualifiedId(children(1)), buildArguments(children(2)))
        case "FieldAccess" => buildFieldAccess(node.children.head)
        case "ArrayAccess" => buildArrayAccess(node.children.head)
        case "MethodInvocation" => buildMethodInvocation(node.children.head)
        case _ => throw AstError(node)
      }
    case TreeNode(Left(value), _) =>
      value match {
        case _: JavaThis => ThisExpr()
        case _: LParen => ParenExpr(buildExpr(node.children(1)))
        case _ => throw AstError(node)
      }
  }

  private def buildMethodInvocation(node: TreeNode): CallExpr = {
    if (node.children.lengthCompare(2) == 0) {
      CallExpr(None, node.children.head.state.left.get.asInstanceOf[Identifier], buildArguments(node.children(1)))
    } else {
      CallExpr(
        Some(buildExpr(node.children.head)),
        node.children(2).state.left.get.asInstanceOf[Identifier],
        buildArguments(node.children(3))
      )
    }
  }

  private def buildArguments(node: TreeNode): List[Expr] = {
    if (node.children.lengthCompare(2) == 0) {
      Nil
    } else {
      buildArgumentsList(node.children(1))
    }
  }

  private def buildArgumentsList(node: TreeNode): List[Expr] = {
    if (node.children.lengthCompare(1) == 0) {
      List(buildExpr(node.children.head))
    } else {
      buildArgumentsList(node.children.head) :+ buildExpr(node.children(2))
    }
  }

  private def buildArrayCreationExpression(node: TreeNode): ArrayNewExpr = {
    val expr = if (node.children.lengthCompare(4) == 0) {
      None
    } else {
      Some(buildExpr(node.children(3)))
    }
    ArrayNewExpr(ArrayType(buildType(node.children(1)), expr))
  }

  private def buildType(node: TreeNode): Type = node match {
    case TreeNode(Right("Type"), children) =>
      buildType(children.head)
    case TreeNode(Right("PrimitiveType"), children) =>
      children.head match {
        case TreeNode(Right(_), grandChildren) =>
          PrimitiveType(grandChildren.head.state.left.get.asInstanceOf[Primitive])
        case TreeNode(Left(value), _) =>
          PrimitiveType(value.asInstanceOf[JavaBoolean])
      }
      //Recursive Case for reference type, since it refers to a type
    case TreeNode(Right("ReferenceType"), children) => buildType(children.head)
    case TreeNode(Right("Name"), _) => ClassType(buildFullyQualifiedId(node))
    case TreeNode(Right("ArrayType"), children) =>
      val arrayType = buildType(children.head)
      ArrayType(arrayType, None)
    case TreeNode(Left(token), _) => throw AstError(token)
    case _ => throw AstError(node)
  }

  private def buildMethodBody(node: TreeNode): Option[BlockStmt] = {
    if (node.children.head.state.isLeft) {
      None
    } else {
      Some(buildBlock(node.children.head))
    }
  }

  private def buildBlock(node: TreeNode): BlockStmt = {
    buildBlockStatement(node.children(1))
  }

  private def buildBlockStatement(node: TreeNode): BlockStmt = {
//    println("0000" + node)
    if (node.children.isEmpty) {
      BlockStmt(Nil)
    } else {
      BlockStmt(buildBlockStatement(node.children.head).stmts :+ buildStatement(node.children(1)))
    }
  }

  private def buildStatement(node: TreeNode): Stmt = {
    node.state match {
      case Right(value) => value match {
        case "LocalVariableDeclaration" =>
          val init = if (node.children.lengthCompare(2) == 0) {
            None
          } else {
            Some(buildExpr(node.children(3)))
          }
          DeclStmt(VarDecl(buildType(node.children.head), node.children(1).state.left.get.asInstanceOf[Identifier]), init)
        case "EmptyStatement" => BlockStmt(Nil)
        case "Block" =>
          buildBlockStatement(node.children(1))
        case "ReturnStatement" =>
          val retVal = if (node.children.lengthCompare(2) == 0) {
            None
          } else {
            Some(buildExpr(node.children(1)))
          }
          ReturnStmt(retVal)
        case "StatementExpression" => ExprStmt(buildExpr(node))
        case "IfThenStatement" => IfStmt(buildExpr(node.children(2)), buildStatement(node.children(4)), None)
        case "IfThenElseStatement" | "IfThenElseStatementNoShortIf" =>
          IfStmt(buildExpr(node.children(2)), buildStatement(node.children(4)), Some(buildStatement(node.children(6))))
        case "WhileStatement" | "WhileStatementNoShortIf" =>
          WhileStmt(buildExpr(node.children(2)), buildStatement(node.children(4)))
        case "ForStatement" | "ForStatementNoShortIf" =>
          ForStmt(buildForInit(node.children(2)), buildForExpr(node.children(4)),
            buildForUpdate(node.children(6)), buildStatement(node.children(8)))
        case _ =>
//          println(node)
          buildStatement(node.children.head)
      }
      case Left(value) => throw AstError(value)
    }
  }

  private def buildForInit(node: TreeNode) : Option[Stmt] = {
    if (node.children.isEmpty) {
      None
    } else {
      node.children.head.state match {
        case Right(value) =>
          value match {
            case "LocalVariableDeclaration" =>
              val declaration = node.children.head
//              println(node)
              val init = if (declaration.children.lengthCompare(2) == 0) {
                None
              } else {
                Some(buildExpr(declaration.children(3)))
              }
              DeclStmt(VarDecl(buildType(declaration.children.head),
                declaration.children(1).state.left.get.asInstanceOf[Identifier]), init)
            case "StatementExpression" => buildExpr(node.children.head)
            case _ => throw AstError(node)
          }
        case Left(token) => throw AstError(token)
      }
      Some(buildStatement(node.children.head))
    }
  }

  private def buildForExpr(node: TreeNode) : Option[Expr] = {
    if (node.children.isEmpty) {
      None
    } else {
      Some(buildExpr(node.children.head))
    }
  }

  private def buildForUpdate(node: TreeNode): Option[Stmt] = {
    if (node.children.isEmpty) {
      None
    } else {
      Some(buildStatement(node.children.head))
    }
  }
  private def buildMethodHeader(node: TreeNode): (Option[Type], Identifier, List[ParameterDecl]) = {
    val typ = node.children.head.state match {
      case Left(_) => None
      case Right(_) => Some(buildType(node.children.head))
    }

    val declarator = node.children(1)
    val identifier = declarator.children.head.state.left.get.asInstanceOf[Identifier]
    val parameters = buildFormalParameters(declarator.children(1))

    (typ, identifier, parameters)
  }

  private def buildFormalParameters(node: TreeNode): List[ParameterDecl] = {
    if (node.children.lengthCompare(2) == 0) {
      Nil
    } else {
      buildParameterList(node.children(1))
    }
  }

  private def buildParameterList(node: TreeNode): List[ParameterDecl] = {
    if (node.children.lengthCompare(1) == 0) {
      List(buildParameterDecl(node.children.head))
    } else {
      buildParameterList(node.children.head) :+ buildParameterDecl(node.children(2))
    }
  }

  private def buildParameterDecl(node: TreeNode): ParameterDecl =
    ParameterDecl(buildType(node.children.head), node.children(1).children.head.state.left.get.asInstanceOf[Identifier])
}

