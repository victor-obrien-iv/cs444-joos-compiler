package HierarchyChecker

sealed trait Signature
case class voidSig() extends Signature
case class ArraySig(typeSig: Signature) extends Signature
case class PrimitiveSig(typ: String)    extends Signature
case class ClassSig(typeDeclHash: Int)  extends Signature
case class MethodSig(name: String, params: List[Signature]) {
  override def toString: String = {
    name + " (" + (for(p <- params) yield p).mkString(", ") + ")"
  }
}
case class ConstructorSig(params: List[Signature])
{
  override def toString: String = {
    "Constructor (" + (for(p <- params) yield p).mkString(", ") + ")"
  }
}