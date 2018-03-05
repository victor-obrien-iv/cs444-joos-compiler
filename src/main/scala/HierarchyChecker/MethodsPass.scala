package HierarchyChecker

import AST._

/**
  * A pass to check the following:
  *   A nonstatic method must not replace a static method
  *   A class that contains (declares or inherits) any abstract methods must be abstract
  *   A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
  *   A class must not declare two constructors with the same parameter types
  *   A method must not replace a method with a different return type
  *   A protected method must not replace a public method
  *   A method must not replace a final method
  *   A class must not declare two methods with the same signature
  *   An interface must not declare two methods with the same signature
  *
  * this pass analyses:
  *
  */
class MethodsPass(localContexts: Map[CompilationUnit, Map[String, TypeDecl]],
                  typeContext: Map[String, List[TypeDecl]], ast: CompilationUnit) extends Visitor  {

  def resolve(fqid: FullyQualifiedID): TypeDecl = {
    def findInPack(name: String, types: List[TypeDecl]): TypeDecl = {
      for(t <- types)
        if(t.name.lexeme == name)
          return t
      assert(assertion = false, s"Type $name does not exist in hierarchy"); types.head
    }
    if ( fqid.qualifiers.nonEmpty )
      findInPack(fqid.id.lexeme, typeContext(fqid.pack))
    else
      localContexts(ast)(fqid.id.lexeme)
  }

  sealed trait Signature
  private case class ArraySig(typeSig: Signature) extends Signature
  private case class PrimitiveSig(typ: String)    extends Signature
  private case class ClassSig(typeDecl: TypeDecl) extends Signature
  private case class MethodSig(name: String, params: List[Signature]/*, retType: Option[Signature]*/)
  private case class CtorSig(params: List[Signature])
  def makeSig(typ: Type): Signature = typ match {
    case ArrayType(arrayOf, _)    => ArraySig(makeSig(arrayOf))
    case PrimitiveType(typeToken) => PrimitiveSig(typeToken.lexeme)
    case ClassType(typeID)        => ClassSig(resolve(typeID))
  }

  // A class must not declare two constructors with the same parameter types
  // A class must not declare two methods with the same signature
  override def visit(cd: ClassDecl): Unit = {
    val methods = cd.members.collect { case md: MethodDecl => md }
    val methodSigs = methods.map { md => MethodSig(md.name.lexeme, for(pd <- md.parameters) yield makeSig(pd.typ)) }
    if(methods.toSet.size != methods.size)
      throw Error.Error(methods.map( md => md.name.lexeme ).mkString(", "),
        "A class or interface must not declare two methods with the same signature",
        Error.Type.MethodsPass, Some(Error.Location(cd.name.row, cd.name.col, ast.fileName)))

    val ctors = cd.members.collect { case cd: ConstructorDecl => cd }
    val ctorSigs = ctors.map { cd => CtorSig(for(p <- cd.parameters) yield makeSig(p.typ)) }
    if(ctorSigs.toSet.size != ctorSigs.size)
      throw Error.Error(ctors.map( cd => cd.identifier.lexeme ).mkString(", "),
        "A class must not declare two constructors with the same parameter types",
        Error.Type.MethodsPass, Some(Error.Location(cd.name.row, cd.name.col, ast.fileName)))

    super.visit(cd)
  }

  // An interface must not declare two methods with the same signature
  override def visit(id: InterfaceDecl): Unit = {
    val methods = id.members.collect { case md: MethodDecl => md }
    val methodSigs = methods.map { md => MethodSig(md.name.lexeme, for(pd <- md.parameters) yield makeSig(pd.typ)) }
    if(methods.toSet.size != methods.size)
      throw Error.Error(methods.map( md => md.name.lexeme ).mkString(", "),
        "A class or interface must not declare two methods with the same signature",
        Error.Type.MethodsPass, Some(Error.Location(id.name.row, id.name.col, ast.fileName)))

    super.visit(id)
  }
}
