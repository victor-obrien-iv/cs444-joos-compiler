package Assembler

import AST._
import Token._

//trait AsmAssembler {
//  /**
//    * pushes vd onto the stack and internally marks where the vd is on the stack
//    * @return the asm that will push the decl onto the stack and assign its value if present
//    */
//  def pushVar(ds: DeclStmt): List[String]
//
//  def getVarAddr(dre: DeclRefExpr): String
//
//  /**
//    * load from the stack
//    * @param dre
//    * @param reg
//    * @return
//    */
//  def loadVar(dre: DeclRefExpr, reg: String): String
//
//  /**
//    * will generate the assembly for the expr, storing the result in eax
//    * @param expr
//    * @param localEnv
//    */
//  def assemble(expr: Expr)(implicit localEnv: List[String]): List[String]
//
////  def assemble(stmt: Stmt)
//
////  def assemble(typ: Type)
//}

class Assembler(num: Int) {
  val labelTag: String = "f" + num.toString

  // 80386 instructions and operands
  object i386 {

    // operands
    private case class Operand(op: String)
    val al = Operand("al")
    val eax = Operand("eax")
    val ebx = Operand("ebx")
    val edx = Operand("edx")
    def constant(c: Int): Operand = Operand(c.toString)
    def variableStackLocation(dre: DeclRefExpr): Operand = {
      assert(assertion = false, "Unimplemented")
      val offset = 0
      Operand(s"dword [ebp+${offset}H]")
    }

    private def twoParamInstr(instr: String)(implicit op1: Operand, op2: Operand) = s"\t$instr\t${op1.op}, ${op2.op}"
    private def oneParamInstr(instr: String)(implicit op1: Operand) = s"\t$instr\t${op1.op}"
    private def zeroParamInstr(instr: String) = s"\t$instr"

    // instructions
    def move(op1: Operand, op2: Operand): String = twoParamInstr("mov")
    def moveZeroExtended(op1: Operand, op2: Operand): String = twoParamInstr("movzx")
    def compare(op1: Operand, op2: Operand): String = twoParamInstr("cmp")
    def push(op1: Operand): String = oneParamInstr("push")
    def pop(op1: Operand): String = oneParamInstr("pop")
    def setOnGreater(op1: Operand): String = oneParamInstr("setg")
    def setOnLess(op1: Operand): String = oneParamInstr("setl")
    def setOnEqual(op1: Operand): String = oneParamInstr("sete")
    def setOnGreaterOrEqual(op1: Operand): String = oneParamInstr("setge")
    def setOnLessOrEqual(op1: Operand): String = oneParamInstr("setle")
    def setOnNotEqual(op1: Operand): String = oneParamInstr("setne")
    def binaryInvert(op1: Operand): String = oneParamInstr("not")
    def binaryAnd(op1: Operand, op2: Operand): String = twoParamInstr("and")
    def binaryOr(op1: Operand, op2: Operand): String = twoParamInstr("or")
    def negate(op1: Operand): String = oneParamInstr("neg")
    def add(op1: Operand, op2: Operand): String = twoParamInstr("add")
    def subtract(op1: Operand, op2: Operand): String = twoParamInstr("sub")
    def signedMultiply(op1: Operand, op2: Operand): String = twoParamInstr("imul")
    def signedDivide(op1: Operand): String = twoParamInstr("idiv")
    def eaxToQuadWord(): String = zeroParamInstr("cdq")
  }

  def pushVar(ds: DeclStmt): List[String] = {
    assert(assertion = false, "Unimplemented")
    List()
  }

  def loadVar(dre: DeclRefExpr, reg: String): String = {
    assert(assertion = false, "Unimplemented")
    ""
  }

  def assemble(stmt: Stmt)(implicit localEnv: List[String]): List[String] = stmt match {
    case BlockStmt(_) =>
      assert(assertion = false, "assemble should not be called on blocks"); List("error")
    case DeclStmt(decl, assignment) =>

    case ExprStmt(expr) =>
      assemble(expr)
    case ReturnStmt(expr) =>
    case _ =>
  }

  def assemble(expr: Expr)(implicit localEnv: List[String]): List[String] = expr match {
    case be: BinaryExpr =>
      assemble(be)
    case ue: UnaryExpr =>
      assemble(ue)
    case pe: ParenExpr =>
      assemble(pe.expr)
    /* TODO:
    case ce: CallExpr =>
    case te: ThisExpr =>
    case ce: CastExpr =>
    case ae: AccessExpr =>
    case aae: ArrayAccessExpr =>
    */
    case ve: ValExpr =>
      assemble(ve)
    /* TODO:
    case dre: DeclRefExpr =>
    case ioe: InstanceOfExpr =>
    case ne: NewExpr =>
    case ne: NamedExpr =>
    */
  }

  def assemble(be: BinaryExpr)(implicit localEnv: List[String]): List[String] = {
    import i386._


    def evaluate(): List[String] =
      assemble(be.lhs) :::
      push(eax) ::
      assemble(be.rhs) :::
      pop(ebx) :: Nil

    def evaluateAndCompare(): List[String] =
      evaluate() :::
      compare(eax, ebx) :: Nil
//
//      def evaluateArithmetic(arithmeticInstr: String): List[String] =
//        evaluate() :::
//        arithmeticInstr + opEaxEbx :: Nil
//
//      def evaluateArithmetic(arithmeticInstrs: List[String]): List[String] =
//        evaluate() :::
//        arithmeticInstrs
//
//      def evaluateComparison(setInstr: String): List[String] =
//        evaluateArithmetic("cmp") :::
//        setInstr + opAl ::
//        moveZeroExtended(opEax, opAl) :: Nil
//
//      def evaluateAssignment(): List[String] =
//        assemble(be.rhs) :::
//        move(getVarAddr(be.lhs.asInstanceOf[DeclRefExpr]), opEax) :: Nil


    be.operatorTok match {
      case Becomes(_, _, _) =>
        assemble(be.rhs) :::
        move(variableStackLocation(be.lhs.asInstanceOf[DeclRefExpr]), eax) :: Nil
      case GT(_, _, _) =>
        evaluateAndCompare() :::
        setOnGreater(al) :: Nil
      case LT(_, _, _) =>
        evaluateAndCompare() :::
        setOnLess(al) :: Nil
      case EQ(_, _, _) =>
        evaluateAndCompare() :::
        setOnEqual(al) :: Nil
      case GE(_, _, _) =>
        evaluateAndCompare() :::
        setOnGreaterOrEqual(al) :: Nil
      case LE(_, _, _) =>
        evaluateAndCompare() :::
        setOnLessOrEqual(al) :: Nil
      case NE(_, _, _) =>
        evaluateAndCompare() :::
        setOnNotEqual(al) :: Nil
      case Bang(_, _, _) =>
        assert(assertion = false, "Bininary bang"); List("error")
      case AmpAmp(_, _, _) =>
        //TODO: add short circuiting
        assert(assertion = false, "need to add short circuiting"); List("error")
      case Amp(_, _, _) =>
        evaluate() :::
        binaryAnd(eax, ebx) :: Nil
      case BarBar(_, _, _) =>
        //TODO: add short circuiting
        assert(assertion = false, "need to add short circuiting"); List("error")
      case Bar(_, _, _) =>
        evaluate() :::
        binaryOr(eax, ebx) :: Nil
      case Plus(_, _, _) =>
        //TODO do type checking for (Str + Str)
        evaluate() :::
        add(eax, ebx) :: Nil
      case Minus(_, _, _) =>
        evaluate() :::
        subtract(eax, ebx) :: Nil
      case Star(_, _, _) =>
        evaluate() :::
        signedMultiply(eax, ebx) :: Nil
      case Slash(_, _, _) =>
        evaluate() :::
        eaxToQuadWord() ::
        signedDivide(ebx) :: Nil
      case Percent(_, _, _) =>
        evaluate() :::
        eaxToQuadWord() ::
        signedDivide(ebx) ::
        move(eax, edx) :: Nil
      case JavaInstanceof(_, _, _) =>
        //TODO: implement instanceof
        assert(assertion = false, "unimplemented"); List("error")
    }
  }

  case class IntMin() extends Exception
  def assemble(ue: UnaryExpr)(implicit localEnv: List[String]): List[String] = {
    import i386._
    ue.operatorTok match {
      case Bang(_, _, _) =>
        assemble(ue.rhs) :::
        compare(eax, constant(0)) ::
        setOnEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case Minus(_, _ ,_) =>
        try
          assemble(ue.rhs) :::
          negate(eax) :: Nil
        catch {
          case IntMin() =>
            move(eax, constant(Int.MinValue)) :: Nil
        }
      case _ =>
        assert(assertion = false, "unexpected unary operator"); List("error")
    }
  }


  def assemble(ve: ValExpr): List[String] = {
    import i386._
//    def make(constant: Int): List[String] =
//      "mov" + opEax(constant) :: Nil

    ve.value match {
      case IntegerLiteral(_, _, _, value) =>
        if (value.isValidInt)
          move(eax, constant(value.intValue())) :: Nil
        else {
          assert(value.intValue() == Int.MinValue, "Oversized int is not intmin?")
          throw IntMin() // prior weeding has asserted that a unary minus comes directly before this
        }
      case BooleanLiteral(_, _, value) =>
        if (value)
          move(eax, constant(1)) :: Nil
        else
          move(eax, constant(0)) :: Nil
      case CharacterLiteral(_, _, _, value) =>
        move(eax, constant(value.toInt)) :: Nil
      case StringLiteral(_, _, _, value) =>
        //TODO: handle strings in expressions
        assert(assertion = false, "strings in expressions not implemented"); List("error")
      case NullLiteral(_, _, _, _) =>
        move(eax, constant(0)) :: Nil
    }
  }
}