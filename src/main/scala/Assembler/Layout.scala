package Assembler

import AST.{FieldDecl, MethodDecl, TypeDecl}
import Disambiguator.TypeChecker
import Token.JavaStatic

class Layout(typeDecl: TypeDecl, typeChecker: TypeChecker) {
  val hierarchy: List[TypeDecl] = {
    def getSuper(decl: TypeDecl): List[TypeDecl] = typeChecker.getSuperClass(decl) match {
      case Some(superClass) => decl :: getSuper(decl)
      case None => decl :: Nil
    }
    (typeDecl :: getSuper(typeDecl)).reverse
  }

  val (fields, methods, _) = typeChecker.partitionMembers(typeDecl.members)

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
  val objectLocation: Map[FieldDecl, Int] = {
    var loc = 0 // start at class vtable ptr location
    def make(decls: List[TypeDecl]): List[(FieldDecl, Int)] = {
      val nonStaticFields = fields.filter(!_.modifiers.exists(_.isInstanceOf[JavaStatic]))
      val fieldLocPairs: List[(FieldDecl, Int)] = for(field <- nonStaticFields) yield {
        loc += i386.wordSize
        (field, loc)
      }
      if(decls.tail.isEmpty)
        fieldLocPairs
      else
        fieldLocPairs ++ make(decls.tail)
    }
    make(hierarchy).toMap
  }


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
  val classLocation: Map[MethodDecl, Int] = {
    var loc = 0 // start at class vtable ptr location
    def make(decls: List[TypeDecl]): List[(MethodDecl, Int)] = {
      val nonStaticMethods = methods.filter(!_.modifiers.exists(_.isInstanceOf[JavaStatic]))
      val MethodLocPairs: List[(MethodDecl, Int)] = for(method <- nonStaticMethods) yield {
        loc += i386.wordSize
        (method, loc)
      }
      if(decls.tail.isEmpty)
        MethodLocPairs
      else
        MethodLocPairs ++ make(decls.tail)
    }
    make(hierarchy).toMap
  }
}
