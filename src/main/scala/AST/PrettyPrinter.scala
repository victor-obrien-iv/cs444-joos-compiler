package AST

import java.io.PrintWriter

import Driver.FileOperations.getFileBaseName
import Token._

class PrettyPrinter(filename: String) extends Visitor {
  private val fileName = s"print/${getFileBaseName(filename)}.ast"
  private val writer = new PrintWriter(fileName, "UTF-8")

  private val numSpaces = 3
  private val indentStr = "|" + (" " * numSpaces)
  private var numIndents = 0

  private def write(str: String): Unit = {
    writer.println(indentStr*numIndents + str)
  }

  private def modStr(mods: List[Modifier]): String = (for(m <- mods) yield { m.lexeme }).mkString(" ")

  override def visit(cu: CompilationUnit): Unit = {
    write(s"Compilation Unit: ${cu.fileName}")
    cu.packageName match {
      case Some(value) => write(s"Package: ${value.name}")
      case None => write("Default package")
    }
    cu.imports foreach { i =>
      write(s"Import: ${i.name.name}")
    }
    numIndents += 1
    super.visit(cu)
    numIndents -= 1
    writer.close()
  }

  override def visit(id: InterfaceDecl): Unit = {
    val str = modStr(id.modifiers)
    write(s"Interface Decl: $str, ${id.name.lexeme} id(${id.id})")
    id.extensionOf foreach { e =>
      write(s"Extends: ${e.name}")
    }
    numIndents += 1
    super.visit(id)
    numIndents -= 1
  }

  override def visit(cd: ClassDecl): Unit = {
    val str = modStr(cd.modifiers)
    write(s"Class Decl: $str, ${cd.name.lexeme} id(${cd.id})")
    cd.extensionOf foreach { e =>
      write(s"Extends: ${e.name}")
    }
    cd.implementationOf foreach { i =>
      write(s"Implements: ${i.name}")
    }
    numIndents += 1
    super.visit(cd)
    numIndents -= 1
  }

  override def visit(cd: ConstructorDecl): Unit = {
    val str = modStr(cd.modifiers)
    write(s"Constructor Decl: $str, ${cd.identifier.lexeme}")
    numIndents += 1
    super.visit(cd)
    numIndents -= 1
  }

  override def visit(fd: FieldDecl): Unit = {
    val str = modStr(fd.modifiers)
    write(s"Field Decl: $str, ${fd.name.lexeme}")
    numIndents += 1
    super.visit(fd)
    numIndents -= 1
  }

  override def visit(md: MethodDecl): Unit = {
    val str = modStr(md.modifiers)
    write(s"Method Decl: $str, ${md.name.lexeme}")
    numIndents += 1
    super.visit(md)
    numIndents -= 1
  }

  override def visit(pd: ParameterDecl): Unit = {
    write(s"Parameter Decl: ${pd.name.lexeme}")
    numIndents += 1
    super.visit(pd)
    numIndents -= 1
  }

  override def visit(vd: VarDecl): Unit = {
    write(s"Var Decl: ${vd.name.lexeme}")
    numIndents += 1
    super.visit(vd)
    numIndents -= 1
  }

  override def visit(be: BinaryExpr): Unit = {
    write(s"Binary Expr: ${be.operatorTok.lexeme}")
    numIndents += 1
    super.visit(be)
    numIndents -= 1
  }

  override def visit(ue: UnaryExpr): Unit = {
    write(s"Binary Expr: ${ue.operatorTok.lexeme}")
    numIndents += 1
    super.visit(ue)
    numIndents -= 1
  }

  override def visit(pe: ParenExpr): Unit = {
    write(s"Paren Expr:")
    numIndents += 1
    super.visit(pe)
    numIndents -= 1
  }

  override def visit(ce: CallExpr): Unit = {
    write(s"Call Expr: ${ce.call.lexeme}")
    numIndents += 1
    super.visit(ce)
    numIndents -= 1
  }

  override def visit(te: ThisExpr): Unit = {
    write("This Expr")
    numIndents += 1
    super.visit(te)
    numIndents -= 1
  }

  override def visit(ce: CastExpr): Unit = {
    write("Cast Expr:")
    numIndents += 1
    super.visit(ce)
    numIndents -= 1
  }

  override def visit(ae: AccessExpr): Unit = {
    write(s"Access Expr: ${ae.field.lexeme}")
    numIndents += 1
    super.visit(ae)
    numIndents -= 1
  }

  override def visit(aae: ArrayAccessExpr): Unit = {
    write("Array Access Expr: (lhs, index)")
    numIndents += 1
    super.visit(aae)
    numIndents -= 1
  }

  override def visit(ve: ValExpr): Unit = {
    write(s"Val Expr: ${ve.value.lexeme}")
    numIndents += 1
    super.visit(ve)
    numIndents -= 1
  }

  override def visit(dre: DeclRefExpr): Unit = {
    write(s"Decl Ref Expr: ${dre.reference.lexeme}")
    numIndents += 1
    super.visit(dre)
    numIndents -= 1
  }

  override def visit(ioe: InstanceOfExpr): Unit = {
    write("InstanceOf Expr: (lhs, typ)")
    numIndents += 1
    super.visit(ioe)
    numIndents -= 1
  }

  override def visit(one: ObjNewExpr): Unit = {
    write(s"Obj New Expr: ${one.ctor.name}")
    numIndents += 1
    super.visit(one)
    numIndents -= 1
  }

  override def visit(ane: ArrayNewExpr): Unit = {
    write("Array New Expr:")
    numIndents += 1
    super.visit(ane)
    numIndents -= 1
  }

  override def visit(ne: NamedExpr): Unit = {
    write(s"Named Expr: ${ne.name.name}")
  }

  override def visit(bs: BlockStmt): Unit = {
    write("Block Stmt:")
    numIndents += 1
    super.visit(bs)
    numIndents -= 1
  }

  override def visit(ds: DeclStmt): Unit = {
    write("Decl Stmt: (decl, expr)")
    numIndents += 1
    super.visit(ds)
    numIndents -= 1
  }

  override def visit(rs: ReturnStmt): Unit = {
    write("Return Stmt:")
    numIndents += 1
    super.visit(rs)
    numIndents -= 1
  }

  override def visit(is: IfStmt): Unit = {
    write("If Stmt: (condition, then, else)")
    numIndents += 1
    super.visit(is)
    numIndents -= 1
  }

  override def visit(fs: ForStmt): Unit = {
    write("For Stmt: (init, condition, update, body)")
    numIndents += 1
    super.visit(fs)
    numIndents -= 1
  }

  override def visit(ws: WhileStmt): Unit = {
    write("While Stmt: (condition, body)")
    numIndents += 1
    super.visit(ws)
    numIndents -= 1
  }

  override def visit(at: ArrayType): Unit = {
    write("ArrayType: (type, size)")
    numIndents += 1
    super.visit(at)
    numIndents -= 1
  }

  override def visit(pt: PrimitiveType): Unit = {
    write(pt.typeToken.lexeme)
  }

  override def visit(ct: ClassType): Unit = {
    write(s"Class: ${ct.typeID.name}")
  }
}
