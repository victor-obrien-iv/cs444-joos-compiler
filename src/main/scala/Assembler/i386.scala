package Assembler

object i386 {
  val wordSize = 4

  // operands
  trait Operand {
    def op: String
  }
  case class Register(op: String) extends Operand
  val al = Register("al")
  val eax = Register("eax")
  val ebx = Register("ebx")
  val ecx = Register("ecx") // reserved for storing rhs in assignment
  val edx = Register("edx")
  val ebp = Register("ebp")
  val esp = Register("esp")

  case class Memory(reg: Operand, offset: Int) extends Operand {
    assert(offset % 4 == 0, "stack location is not word aligned")
    val op: String =
    if (offset > 0)
      s"dword [${reg.op}+$offset]"
    else if (offset < 0)
      s"dword [${reg.op}$offset]" // int.toString will put in the minus
    else
      s"dword [${reg.op}]"
  }
  def stackMemory(offset: Int): Memory = Memory(ebp, offset)

  case class Immediate(value: Int) extends Operand {
    val op: String = value.toString

  }

  case class Data(label: Label) extends Operand {
    val op = s"dword [${label.name}]"
  }

  private def instr(instr: String, op1: Operand, op2: Operand) = s"\t$instr\t${op1.op}, ${op2.op}"
  private def instr(instr: String, op1: Operand, label: Label) = s"\t$instr\t${op1.op}, ${label.name}"
  private def instr(instr: String, op1: Operand) = s"\t$instr\t${op1.op}"
  private def instr(instr: String, label: Label) = s"\t$instr\t${label.name}"
  private def instr(instr: String) = s"\t$instr"
  def NoOperation(): String = instr("nop")

  // labels
  def placeLabel(label: Label): String = s"${label.name}:"

  // comments
  def comment(message: String): String = s"\t; $message"

  // movement instrs
  def move(op1: Operand, op2: Operand): String = instr("mov", op1, op2)
  def move(op1: Operand, label: Label): String = instr("mov", op1, label)
  def moveZeroExtended(op1: Operand, op2: Operand): String = instr("movzx", op1, op2)
  def loadEffectiveAddress(op1: Operand, label: Label): String = instr("lea", op1, label)
  def loadFromObject(objPtr: Memory, offset: Int): List[String] =
    move(eax, objPtr) ::
    move(eax, Memory(eax, offset)) + comment("load from object") :: Nil
  def loadFromObject(objPtr: Register, offset: Int): String =
    move(eax, Memory(objPtr, offset)) + comment("load from object")
  def storeIntoObject(objPtr: Register, offset: Int, value: Register): String =
    move(Memory(objPtr, offset), value) + comment("store into object")
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
    move(eax, edx) + comment("signed modulo") :: Nil

  // control flow instrs
  private def leave(): String = instr("leave")
  def jump(destination: Label): String = instr("jmp", destination)
  private def jumpIfZero(destination: Label): String = instr("jz", destination)
  private def jumpIfNotZero(destination: Label): String = instr("jnz", destination)
  def jumpIfRegIsTrue(op1: Operand, destination: Label): List[String] =
    compare(op1, Immediate(0)) ::
    jumpIfNotZero(destination) :: Nil
  def jumpIfRegIsFalse(op1: Operand, destination: Label): List[String] =
    compare(op1, Immediate(0)) ::
    jumpIfZero(destination) :: Nil
  def call(destination: Label): String = instr("call", destination)
  def call(reg: Register): String = instr("call", reg)
  def discardArgs(numArgs: Int): String = add(esp, Immediate(4 * numArgs))
  def functionEntrance(enter: Label, totalLocalBytes: Int): List[String] =
    placeLabel(enter) ::
    push(ebp) ::
    move(ebp, esp) ::
    subtract(esp, Immediate(totalLocalBytes)) + comment("end of function entrance") :: Nil
  private def return_(): String = instr("ret")
  def functionExit(): List[String] =
    leave() ::
    return_() :: Nil

  // runtime calls
  def allocate(numBytes: Int): List[String] =
    move(eax, Immediate(numBytes)) ::
    call(LabelFactory.mallocLabel) :: Nil
  def nullCheck(): List[String] =
    NoOperation() :: Nil
//    comment("null check") ::
//    jumpIfRegIsFalse(eax, LabelFactory.exceptionLabel)
  def debugExit(): String =
    call(LabelFactory.debugExitLabel) + comment("exit")

  // data
  def placeValue(label: Label): String = s"dd ${label.name}"
  def placeValue(value: Int): String = s"dd $value"
}
