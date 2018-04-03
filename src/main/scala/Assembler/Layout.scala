package Assembler

import AST.{FieldDecl, MethodDecl, TypeDecl}
import Disambiguator.TypeChecker
import Token.JavaStatic

class Layout(typeDecl: TypeDecl, typeChecker: TypeChecker) {

  /**
    *
    */
  val hierarchy: List[(TypeDecl, (List[FieldDecl], List[MethodDecl]))] = getHierarchy(typeDecl).reverse
  private def getHierarchy(decl: TypeDecl): List[(TypeDecl, (List[FieldDecl], List[MethodDecl]))] = {
    val (fields, methods, _) = typeChecker.partitionMembers(decl.members)
    typeChecker.getSuperClass(decl) match {
      case Some(superClass) => (decl, (fields, methods)) :: getHierarchy(superClass)
      case None => (decl, (fields, methods)) :: Nil
    }
  }

  /*
  Object layout:
  [ 0 ] = class vtable ptr
  [ 4 ]
    :   = super class non-static fields
  [ X ]
  [X+4]
    :   = non-static fields
  [ Y ]
   */
  /**
    * Maps each non-static field to its location in the object
    */
  val objectLayout: Map[FieldDecl, Int] = makeFieldLayout(hierarchy).toMap
  def objectLayout(typeDecl: TypeDecl): Map[FieldDecl, Int] =
    if (typeDecl eq this.typeDecl) objectLayout
    else makeFieldLayout(getHierarchy(typeDecl)).toMap
  private def makeFieldLayout(decls: List[(TypeDecl, (List[FieldDecl], List[MethodDecl]))]) = {
    def make(decls: List[(TypeDecl, (List[FieldDecl], List[MethodDecl]))], loc: Int):
    List[(FieldDecl, Int)] = {
      val fields = decls.head._2._1
      val nonStaticFields = fields.filter(!_.modifiers.exists(_.isInstanceOf[JavaStatic])) //TODO fix this
      val fieldLocPairs = for(field <- nonStaticFields) yield { (field, loc)}
      if(decls.tail.isEmpty)
        fieldLocPairs
      else
        fieldLocPairs ++ make(decls.tail, loc + i386.wordSize)
    }
    make(decls, 4)
  }

  /**
    * 4 bytes for the vtable ptr
    * 4 bytes for each field going up the hierarchy
    */
  val getObjByteSize: Int = objByteSize(objectLayout.size)
  def getObjByteSize(typeDecl: TypeDecl): Int =
    if (typeDecl eq this.typeDecl) getObjByteSize
    else objByteSize(makeFieldLayout(getHierarchy(typeDecl)).size)
  private def objByteSize(numFields: Int): Int = 4 + (4 * numFields)

  /*
  Class layout:
  [ 0 ] = type table for this class
  [ 4 ]
    :   = super class non-static methods / override methods
  [ X ]
  [X+4]
    :   = non-static methods
  [ Y ]
   */
  /**
    * Maps each non-static method to its location in the class
    */
  val classLayout: Map[MethodDecl, Int] = makeClassLayout(hierarchy).toMap
  def classLayout(typeDecl: TypeDecl): Map[MethodDecl, Int] =
    if (typeDecl eq this.typeDecl) classLayout
    else makeClassLayout(getHierarchy(typeDecl)).toMap
  private def makeClassLayout(decls: List[(TypeDecl, (List[FieldDecl], List[MethodDecl]))]) = {
    def make(decls: List[(TypeDecl, (List[FieldDecl], List[MethodDecl]))], loc: Int):
    List[(MethodDecl, Int)] = {
      val methods = decls.head._2._2
      val nonStaticMethods = methods.filter(!_.modifiers.exists(_.isInstanceOf[JavaStatic])) //TODO fix this
      val MethodLocPairs = for(method <- nonStaticMethods) yield { (method, loc) }
      if(decls.tail.isEmpty)
        MethodLocPairs
      else
        MethodLocPairs ++ make(decls.tail, loc + i386.wordSize)
    }
    make(decls, 4)
  }

}
