package Assembler

import java.util.concurrent.atomic.AtomicInteger

import AST._

case class Label(name: String)

object LabelFactory {
  val labelID = new AtomicInteger(0)
  def makeLocalLabel(use: String): Label = {
    val i = labelID.getAndAdd(1)
    Label(s"${use}_$i")
  }
  def makeLocalLabels(uses: List[String]): List[Label] = {
    val i = labelID.getAndAdd(1)
    for(use <- uses) yield { Label(s"${use}_$i") }
  }
}

//TODO: gonna need to change this so a cu can get labels outside its own
class LabelFactory(cu: CompilationUnit) {
  private val labelPrefix = cu.packageName match {
    case Some(value) => s"${value.name}_${cu.typeDecl.name.lexeme}"
    case None => s"?_${cu.typeDecl.name.lexeme}"
  }

  private var globalLabels = List[Label]()
  def getGlobalLabels: List[Label] = globalLabels
  private var externLabels = List[Label]()
  def getExternLabels: List[Label] = externLabels

  private def typeName(t: Type): String = t match {
    case ArrayType(arrayOf, _) =>
      s"ARRAY_${typeName(arrayOf)}"
    case PrimitiveType(typeToken) =>
      typeToken.lexeme
    case ClassType(typeID) =>
      s"CLASS_${typeID.name}"
  }

  def makeMethodLabel(md: MethodDecl): Label = {
    val params = (for(p <- md.parameters) yield { typeName(p.typ) }).mkString("~")
    globalLabels = Label(s"${labelPrefix}_METHOD_${md.name.lexeme}~$params") :: globalLabels
    globalLabels.head
  }

  def makeCtorLabel(cd: ConstructorDecl): Label = {
    val params = (for(p <- cd.parameters) yield { typeName(p.typ) }).mkString("~")
    globalLabels = Label(s"${labelPrefix}_CTOR_${cd.identifier.lexeme}~$params") :: globalLabels
    globalLabels.head
  }

  def makeFieldLabel(fd: FieldDecl): Label = {
    globalLabels = Label(s"${labelPrefix}_FIELD_${fd.name}") :: globalLabels
    globalLabels.head
  }
}
