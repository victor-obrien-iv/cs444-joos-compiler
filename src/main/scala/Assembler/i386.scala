package Assembler

import AST.DeclRefExpr

object i386 {

  // operands
  case class Operand(op: String)
  val al = Operand("al")
  val eax = Operand("eax")
  val ebx = Operand("ebx")
  val edx = Operand("edx")
  def constant(c: Int): Operand = Operand(c.toString)
  private def label(l: String): Operand = Operand(l)
  def variableStackLocation(dre: DeclRefExpr): Operand = {
    assert(assertion = false, "Unimplemented")
    val offset = 0
    Operand(s"dword [ebp+${offset}H]")
  }

  private def instr(instr: String, op1: Operand, op2: Operand) = s"\t$instr\t${op1.op}, ${op2.op}"
  private def instr(instr: String, op1: Operand) = s"\t$instr\t${op1.op}"
  private def instr(instr: String) = s"\t$instr"

  // labels
  private def placeLabel(label: String): String = s"$label: "
  def placeLabel(label: String, comment: String): String = s"${placeLabel(label)}\t;$comment"
  def prependLabel(label: String, instrs: List[String]): List[String] = {
    val newHead = placeLabel(label) + instrs.head
    newHead :: instrs.drop(1)
  }

  // movement instrs
  def move(op1: Operand, op2: Operand): String = instr("mov", op1, op2)
  def moveZeroExtended(op1: Operand, op2: Operand): String = instr("movzx", op1, op2)
  def push(op1: Operand): String = instr("push", op1)
  def pop(op1: Operand): String = instr("pop", op1)

  // comparison instrs
  def compare(op1: Operand, op2: Operand): String = instr("cmp", op1, op2)
  def setOnGreater(op1: Operand): String = instr("setg", op1)
  def setOnLess(op1: Operand): String = instr("setl", op1)
  def setOnEqual(op1: Operand): String = instr("sete", op1)
  def setOnGreaterOrEqual(op1: Operand): String = instr("setge", op1)
  def setOnLessOrEqual(op1: Operand): String = instr("setle", op1)
  def setOnNotEqual(op1: Operand): String = instr("setne", op1)

  // binary instrs
  def binaryInvert(op1: Operand): String = instr("not", op1)
  def binaryAnd(op1: Operand, op2: Operand): String = instr("and", op1, op2)
  def binaryOr(op1: Operand, op2: Operand): String = instr("or", op1, op2)

  // arithmetic instrs
  def negate(op1: Operand): String = instr("neg", op1)
  def add(op1: Operand, op2: Operand): String = instr("add", op1, op2)
  def subtract(op1: Operand, op2: Operand): String = instr("sub", op1, op2)
  def signedMultiply(op1: Operand, op2: Operand): String = instr("imul", op1, op2)
  def signedDivide(op1: Operand): String = instr("idiv", op1)
  def eaxToQuadWord(): String = instr("cdq")

  // control flow instrs
  def enter(numBytes: Int): String = instr("enter", constant(numBytes))
  def leave(): String = instr("leave")
  def jump(destinationLabel: String): String = instr("jmp", label(destinationLabel))
  private def jumpIfZero(destinationLabel: String): String = instr("jz", label(destinationLabel))
  private def jumpIfNotZero(destinationLabel: String): String = instr("jnz", label(destinationLabel))
  def jumpIfRegIsTrue(op1: Operand, destinationLabel: String): List[String] = {
    compare(op1, constant(0)) ::
    jumpIfNotZero(destinationLabel) :: Nil
  }
  def jumpIfRegIsFalse(op1: Operand, destinationLabel: String): List[String] = {
    compare(op1, constant(0)) ::
    jumpIfZero(destinationLabel) :: Nil
  }
  def return_(): String = instr("ret")
}
