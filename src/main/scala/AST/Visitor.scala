package AST

import akka.actor.Actor

import scala.util.Try

abstract class Visitor extends Actor {
  def visit(n: AstNode): Unit = n match {
    case d: Decl => visit(d)
    case s: Stmt => visit(s)
    case t: Type => visit(t)
  }

      //
      //  DECLARATION VISITS
      //
  def visit(d: Decl): Unit = d match {
    case cu: CompilationUnit => visit(cu: CompilationUnit)
    case id: ImportDecl =>      visit(id: ImportDecl)
    case id: InterfaceDecl =>   visit(id: InterfaceDecl)
    case cd: ClassDecl =>       visit(cd: ClassDecl)
    case cd: ConstructorDecl => visit(cd: ConstructorDecl)
    case fd: FieldDecl =>       visit(fd: FieldDecl)
    case md: MethodDecl =>      visit(md: MethodDecl)
    case pd: ParameterDecl =>   visit(pd: ParameterDecl)
    case vd: VarDecl =>         visit(vd: VarDecl)
  }
  def visit(cu: CompilationUnit): Unit = {
    cu.packageName match {
      case Some(pn) => visit(pn: FullyQualifiedID)
      case None =>
    }
    for(i <- cu.imports) visit(i: ImportDecl)
    for(i <- cu.interfaces) visit(i: InterfaceDecl)
    for(cd <- cu.classes) visit(cd: ClassDecl)
  }
  def visit(id: ImportDecl): Unit = {
    visit(id.name: FullyQualifiedID)
  }
  def visit(id: InterfaceDecl): Unit = {
    for(d <- id.members) visit(d: Decl)
  }
  def visit(cd: ClassDecl): Unit = {
    cd.extensionOf match {
      case Some(fqid) => visit(fqid: FullyQualifiedID)
      case None =>
    }
    for(fqid <- cd.implementationOf) visit(fqid: FullyQualifiedID)
    for(d <- cd.members) visit(d: Decl)
  }
  def visit(cd: ConstructorDecl): Unit = {
    for(p <- cd.parameters) visit(p: ParameterDecl)
    visit(cd.body: BlockStmt)
  }
  def visit(fd: FieldDecl): Unit = {
    visit(fd.typ: Type)
    fd.assignment match {
      case Some(e) => visit(e: Expr)
      case None =>
    }
  }
  def visit(md: MethodDecl): Unit = {
    md.returnType match {
      case Some(t) => visit(t: Type)
      case None =>
    }
    for(p <- md.parameters) visit(p: ParameterDecl)
    md.body match {
      case Some(bs) => visit(bs: BlockStmt)
      case None =>
    }
  }
  def visit(pd: ParameterDecl): Unit = {
    visit(pd.typ: Type)
  }
  def visit(vd: VarDecl): Unit = {
    visit(vd.typ: Type)
  }

      //
      //  EXPRESSION VISITS
      //
  def visit(e: Expr): Unit = e match {
    case be:  BinaryExpr      => visit(be: BinaryExpr)
    case ue:  UnaryExpr       => visit(ue: UnaryExpr)
    case ce:  CallExpr        => visit(ce: CallExpr)
    case te:  ThisExpr        => visit(te: ThisExpr)
    case ce:  CastExpr        => visit(ce: CastExpr)
    case ae:  AccessExpr      => visit(ae: AccessExpr)
    case aae: ArrayAccessExpr => visit(aae: ArrayAccessExpr)
    case ve:  ValExpr         => visit(ve: ValExpr)
    case dre: DeclRefExpr     => visit(dre: DeclRefExpr)
    case ioe: InstanceOfExpr  => visit(ioe: InstanceOfExpr)
    case ne:  NewExpr         => visit(ne: NewExpr)
    case ne:  NamedExpr       => visit(ne: NamedExpr)
    case pe:  ParenExpr       => visit(pe: ParenExpr)
  }
  def visit(be: BinaryExpr): Unit = {
    visit(be.lhs: Expr)
    visit(be.rhs: Expr)
  }
  def visit(ue: UnaryExpr): Unit = {
    visit(ue.rhs)
  }
  def visit(pe: ParenExpr): Unit = {
    visit(pe.expr: Expr)
  }
  def visit(ce: CallExpr): Unit = {
    ce.obj match {
      case Some(e) => visit(e: Expr)
      case None =>
    }
    for(e <- ce.params) visit(e: Expr)
  }
  def visit(te: ThisExpr): Unit = {
  }
  def visit(ce: CastExpr): Unit = {
    visit(ce.castType: Type)
    visit(ce.rhs: Expr)
  }
  def visit(ae: AccessExpr): Unit = {
    visit(ae.lhs: Expr)
  }
  def visit(aae: ArrayAccessExpr): Unit = {
    visit(aae.lhs)
  }
  def visit(ve: ValExpr): Unit = {
  }
  def visit(dre: DeclRefExpr): Unit = {
  }
  def visit(ioe: InstanceOfExpr): Unit = {
    visit(ioe.lhs: Expr)
    visit(ioe.typ: Type)
  }
  def visit(ne:  NewExpr): Unit = ne match {
    case one: ObjNewExpr => visit(one: ObjNewExpr)
    case ane: ArrayNewExpr => visit(ane: ArrayNewExpr)
  }
  def visit(one: ObjNewExpr): Unit = {
    visit(one.ctor: FullyQualifiedID)
    for(p <- one.params) visit(p: Expr)
  }
  def visit(ane: ArrayNewExpr): Unit = {
    visit(ane.arrayType: ArrayType)
  }
  def visit(ne: NamedExpr): Unit = {
    visit(ne.name: FullyQualifiedID)
  }

      //
      //  STATEMENT VISITS
      //
  def visit(s: Stmt): Unit = s match {
    case bs: BlockStmt => visit(bs: BlockStmt)
    case ds: DeclStmt => visit(ds: DeclStmt)
    case es: ExprStmt => visit(es: ExprStmt)
    case rs: ReturnStmt => visit(rs: ReturnStmt)
    case cfs: CtrlFlowStmt => visit(cfs: CtrlFlowStmt)
  }
  def visit(bs: BlockStmt): Unit = {
    for(s <- bs.stmts) visit(s: Stmt)
  }
  def visit(ds: DeclStmt): Unit = {
    visit(ds.decl: Decl)
    ds.assignment match {
      case Some(e) => visit(e: Expr)
      case None =>
    }
  }
  def visit(es: ExprStmt): Unit = {
    visit(es.expr: Expr)
  }
  def visit(rs: ReturnStmt): Unit = rs.expr match {
    case Some(e) => visit(e: Expr)
    case None =>
  }
  def visit(cfs: CtrlFlowStmt): Unit = cfs match {
    case is: IfStmt => visit(is: IfStmt)
    case ls: LoopStmt => visit(ls: LoopStmt)
  }
  def visit(is: IfStmt): Unit = {
    visit(is.condition: Expr)
    visit(is.thenStmt: Stmt)
    is.elseStmt match {
      case Some(s) => visit(s: Stmt)
      case None =>
    }
  }
  def visit(ls: LoopStmt): Unit = ls match {
    case fs: ForStmt => visit(fs: ForStmt)
    case ws: WhileStmt => visit(ws: WhileStmt)
  }
  def visit(fs: ForStmt): Unit = {
    fs.init match {
      case Some(s) => visit(s: Stmt)
      case None =>
    }
    fs.condition match {
      case Some(e) => visit(e: Expr)
      case None =>
    }
    fs.update match {
      case Some(s) => visit(s: Stmt)
      case None =>
    }
    visit(fs.bodyStmt: Stmt)
  }
  def visit(ws: WhileStmt): Unit = {
    visit(ws.condition: Expr)
    visit(ws.bodyStmt: Stmt)
  }

      //
      //  TYPE VISITS
      //
  def visit(t: Type): Unit = t match {
    case at: ArrayType => visit(at: ArrayType)
    case pt: PrimitiveType => visit(pt: PrimitiveType)
    case ct: ClassType => visit(ct: ClassType)
  }
  def visit(at: ArrayType): Unit = {
    visit(at.arrayOf: Type)
    at.size match {
      case Some(e) => visit(e: Expr)
      case None =>
    }
  }
  def visit(pt: PrimitiveType): Unit = {
  }
  def visit(ct: ClassType): Unit = {
    visit(ct.typeID: FullyQualifiedID)
  }

      //
      //  ACCESSORY VISITS
      //
  def visit(fqid: FullyQualifiedID): Unit = {
  }

  override def receive: Receive = {
    case cu: CompilationUnit =>
      sender ! Try(visit(cu: CompilationUnit))
      context.stop(self)
    case _ => assert(assertion = false, "Visitor must start on a compilation unit!")
  }
}
