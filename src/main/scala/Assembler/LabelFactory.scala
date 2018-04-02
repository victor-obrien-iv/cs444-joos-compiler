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

  def makeClassLabel(typeDecl: TypeDecl): Label = {
    globalLabels =  classLabel(typeDecl) :: globalLabels
    globalLabels.head
  }

  private def classLabel(typeDecl: TypeDecl): Label = {
    Label(s"${labelPrefix(typeDecl)}")
  }

  def makeVtableLabel(typeDecl: TypeDecl): Label = {
    globalLabels = vtableLabel(typeDecl) :: globalLabels
    globalLabels.head
  }

  private def vtableLabel(typeDecl: TypeDecl) = {
    Label(s"${labelPrefix(typeDecl)}_VTABLE")
  }

  def makeMethodLabel(md: MethodDecl, typeDecl: TypeDecl): Label = {
    globalLabels = methodLabel(md, typeDecl) :: globalLabels
    globalLabels.head
  }

  private def methodLabel(md: MethodDecl, typeDecl: TypeDecl) = {
    val params = (for(p <- md.parameters) yield { typeName(p.typ) }).mkString("~")
    Label(s"${labelPrefix(typeDecl)}_METHOD_${md.name.lexeme}~$params")
  }

  def makeCtorLabel(cd: ConstructorDecl, typeDecl: TypeDecl): Label = {
    globalLabels = ctorLabel(cd, typeDecl) :: globalLabels
    globalLabels.head
  }

  private def ctorLabel(cd: ConstructorDecl, typeDecl: TypeDecl) = {
    val params = (for(p <- cd.parameters) yield { typeName(p.typ) }).mkString("~")
    Label(s"${labelPrefix(typeDecl)}_CTOR_${cd.identifier.lexeme}~$params")
  }

  def makeFieldLabel(fd: FieldDecl, typeDecl: TypeDecl): Label = {
    globalLabels = Label(s"${labelPrefix(typeDecl)}_FIELD_${fd.name}") :: globalLabels
    globalLabels.head
  }
}
