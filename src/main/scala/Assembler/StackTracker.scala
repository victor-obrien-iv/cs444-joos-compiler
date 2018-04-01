package Assembler

import AST.{ParameterDecl, VarDecl}
import Token.Identifier

import scala.collection.mutable


class StackTracker private (outerScope: Option[StackTracker]) {
  def this(outerScope: StackTracker) = this(Some(outerScope))
  def this(params: List[ParameterDecl], inObject: Boolean) = {
    this(None)
    var index = 8 // start at 1st argument
    if (inObject) {
      varsInBlock(thisReference) = index
      index += i386.wordSize
    }
    for(p <- params) {
      val varName = p.name.lexeme
      assert(assertion = !varsInBlock.contains(varName), s"Variable $varName already pushed on stack")
      varsInBlock(varName) = index
      index += i386.wordSize
    }
  }

  /*
  [ebp + 12] (2nd argument)
  [ebp + 8]  (1st argument)
  [ebp + 4]  (return address)
  [ebp]      (old ebp value)
  [ebp - 4]  (1st local variable)
  [ebp - 8]  (2nd local variable)
   */
  private var stackIndex = -4 // start at 1st local variable
  private val thisReference = "0"

  private val varsInBlock = mutable.Map[String, Int]()
  private def push(varName: String): Unit = {
    assert(assertion = !varsInBlock.contains(varName), s"Variable $varName already pushed on stack")
    varsInBlock(varName) = stackIndex
    stackIndex -= i386.wordSize
  }

  def pushInternalVar(id: Int): Unit = push(id.toString)
  def pushVar(vd: VarDecl): Unit = push(vd.name.lexeme)

  case class varNotFound() extends Exception
  private def lookUpLocation(name: String): Int = {
    if(!varsInBlock.contains(name)) outerScope match {
      case Some(scope) => scope.lookUpLocation(name)
      case None => throw varNotFound()
    }
    else varsInBlock(name)
  }
  def lookUpLocation(id: Identifier): Int = lookUpLocation(id.lexeme)
  def lookUpThis(): Int = lookUpLocation(thisReference)
}
