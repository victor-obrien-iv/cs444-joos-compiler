package Assembler

import AST.TypeDecl

object i386 {
  val wordSize = 4

  // operands
  case class Operand(op: String)
  val al = Operand("al")
  val eax = Operand("eax")
  val ebx = Operand("ebx")
  val edx = Operand("edx")
  val ebp = Operand("ebp")
  val esp = Operand("esp")
  def constant(c: Int): Operand = Operand(c.toString)
  def stackAddress(offset: Int): Operand = {
    assert(offset % 4 == 0, "stack location is not word aligned")
    if(offset >= 0) Operand(s"dword [ebp+$offset]")
    else Operand(s"dword [ebp$offset]") // scala toString will put in the minus
  }

  private def instr(instr: String, op1: Operand, op2: Operand) = s"\t$instr\t${op1.op}, ${op2.op}"
  private def instr(instr: String, op1: Operand) = s"\t$instr\t${op1.op}"
  private def instr(instr: String, label: Label) = s"\t$instr\t${label.name}"
  private def instr(instr: String) = s"\t$instr"

  // labels
  def placeLabel(label: Label): String = s"${label.name}:"

  // comments
  def comment(message: String): String = s"\t; $message"

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
  private def eaxToQuadWord(): String = instr("cdq")
  def signedDivide(op1: Operand): List[String] =
    instr("cdq") ::
    instr("idiv", op1) :: Nil
  def signedModulo(op1: Operand): List[String] =
    signedDivide(op1) :::
    move(eax, edx) :: Nil

  // control flow instrs
  private def leave(): String = instr("leave")
  def jump(destination: Label): String = instr("jmp", destination)
  private def jumpIfZero(destination: Label): String = instr("jz", destination)
  private def jumpIfNotZero(destination: Label): String = instr("jnz", destination)
  def jumpIfRegIsTrue(op1: Operand, destination: Label): List[String] =
    compare(op1, constant(0)) ::
    jumpIfNotZero(destination) :: Nil
  def jumpIfRegIsFalse(op1: Operand, destination: Label): List[String] =
    compare(op1, constant(0)) ::
    jumpIfZero(destination) :: Nil
  def call(destination: Label): String = instr("call", destination)
  def functionEntrance(enter: Label, paramTotalBytes: Int): List[String] =
    placeLabel(enter) ::
    push(ebp) ::
    move(ebp, esp) ::
    subtract(esp, constant(paramTotalBytes)) :: Nil
  private def return_(): String = instr("ret")
  def functionExit(): List[String] =
    leave() ::
    return_() :: Nil

  // runtime calls
  def allocate(numBytes: Int): List[String] =
    move(eax, constant(numBytes)) ::
    call(Label("__malloc")) :: Nil
  def nullCheck(): List[String] =
    jumpIfRegIsFalse(eax, LabelFactory.exceptionLabel)

  // data
  def placeValue(label: Label): String = s"dd ${label.name}"
  def placeValue(value: Int): String = s"dd $value"
}
