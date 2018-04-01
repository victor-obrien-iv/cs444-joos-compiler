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

  def labelPrefix(typeDecl: TypeDecl): String = typeDecl.packageName match {
    case Some(value) => s"${value.name}_${typeDecl.name.lexeme}"
    case None => s"?_${typeDecl.name.lexeme}"
  }

  private var globalLabels = List[Label]()
  def getGlobalLabels: List[Label] = globalLabels
  private var externLabels = List[Label]()
  def getExternLabels: List[Label] = externLabels

  private def typeName(t: Type): String = t match {
    case rt: ReferenceType => rt match {
      case ArrayType(arrayOf, _) => s"ARRAY_${typeName(arrayOf)}"
      case ClassType(typeID) => s"CLASS_${typeID.name}"
      case NullType() | Class(_)=> throw Error.Error.undefinedMatch
    }
    case PrimitiveType(typeToken) => typeToken.lexeme
  }

  def makeMethodLabel(md: MethodDecl, typeDecl: TypeDecl): Label = {
    val params = (for(p <- md.parameters) yield { typeName(p.typ) }).mkString("~")
    globalLabels = Label(s"${labelPrefix(typeDecl)}_METHOD_${md.name.lexeme}~$params") :: globalLabels
    globalLabels.head
  }

  def makeCtorLabel(cd: ConstructorDecl, typeDecl: TypeDecl): Label = {
    val params = (for(p <- cd.parameters) yield { typeName(p.typ) }).mkString("~")
    globalLabels = Label(s"${labelPrefix(typeDecl)}_CTOR_${cd.identifier.lexeme}~$params") :: globalLabels
    globalLabels.head
  }

  def makeFieldLabel(fd: FieldDecl, typeDecl: TypeDecl): Label = {
    globalLabels = Label(s"${labelPrefix(typeDecl)}_FIELD_${fd.name}") :: globalLabels
    globalLabels.head
  }
}
