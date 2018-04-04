package Assembler

import java.util.concurrent.atomic.AtomicInteger
import AST._
import scala.collection.mutable

case class Label(name: String)

object LabelFactory {
  val labelID = new AtomicInteger(0)

  def makeLocalLabel(use: String): Label = {
    val i = labelID.getAndAdd(1)
    Label(s"${use}_$i")
  }

  def makeLocalLabels(uses: List[String]): List[Label] = {
    val i = labelID.getAndAdd(1)
    for (use <- uses) yield {
      Label(s"${use}_$i")
    }
  }
  def makeSubTypeTableEntryLabel(i: Int) = Label(s"subtype_table_type$i")

  val mallocLabel = Label("__malloc")
  val debugExitLabel = Label("__debexit")
  val exceptionLabel = Label("__exception")
  val nativeWriteLabel = Label("NATIVEjava.io.OutputStream.nativeWrite")
}

class LabelFactory(thisType: TypeDecl) {

  /**
    * the LabelFactory manages where labels are defined
    *   those defined in thisType must be made global so other classes can find it
    *   those defined in another class must be made extern so the linker knows it is externally defined
    */
  private val globalLabels: mutable.Set[Label] = mutable.Set()
  private val externLabels: mutable.Set[Label] = mutable.Set(
    LabelFactory.mallocLabel,
    LabelFactory.debugExitLabel,
    LabelFactory.exceptionLabel,
    LabelFactory.nativeWriteLabel,
  )
  private def addLabel(l: Label, td: TypeDecl) =
    if (td eq thisType) globalLabels.add(l)
    else externLabels.add(l)
  def exportLabels(): List[String] = {
    val globals: List[String] = (for(g <- globalLabels) yield { s"global ${g.name}"}).toList
    val externs: List[String] = (for(e <- externLabels) yield { s"extern ${e.name}"}).toList
    globals ::: externs
  }


  private def labelPrefix(typeDecl: TypeDecl): String = typeDecl.packageName match {
    case Some(value) => s"${value.name}_${typeDecl.name.lexeme}"
    case None => s"?_${typeDecl.name.lexeme}"
  }

  private def typeName(t: Type): String = t match {
    case rt: ReferenceType => rt match {
      case ArrayType(arrayOf, _) => s"ARRAY_${typeName(arrayOf)}"
      case ClassType(typeID) => s"CLASS_${typeID.name}"
      case NullType() | Class(_)=> throw Error.Error.undefinedMatch
    }
    case PrimitiveType(typeToken) => typeToken.lexeme
  }

  def makeStartLabel(): Label = {
    val label = Label("_start")
    globalLabels.add(label)
    label
  }

  def makeLabel(originType: TypeDecl, md: MemberDecl): Label = {
    val label = md match {
      case ConstructorDecl(_, identifier, parameters, _) =>
        val params = (for (p <- parameters) yield {
          typeName(p.typ)
        }).mkString("~")
        Label(s"${labelPrefix(originType)}_CTOR_${identifier.lexeme}~$params")
      case FieldDecl(_, _, name, _) =>
        Label(s"${labelPrefix(originType)}_FIELD_${name.lexeme}")
      case MethodDecl(_, _, name, parameters, _) =>
        val params = (for (p <- parameters) yield {
          typeName(p.typ)
        }).mkString("~")
        Label(s"${labelPrefix(originType)}_METHOD_${name.lexeme}~$params")
    }
    addLabel(label, originType)
    label
  }

  def makeClassLabel(originType: TypeDecl): Label = {
    val label = Label(s"${labelPrefix(originType)}")
    addLabel(label, originType)
    label
  }

  def makeVtableLabel(originType: TypeDecl): Label = {
    val label = Label(s"${labelPrefix(originType)}_VTABLE")
    addLabel(label, originType)
    label
  }

  def makeSubTypeTableEntryLabel(i: Int): Label = {
    val label = Label(s"subtype_table_type$i")
    globalLabels += label
    label
  }

}
