package Assembler

import AST._
import Token._
import i386._

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
  val labelTag: String = s"F${num.toString}_"
  var labelCount: Int = 0
  def makeLabel(usage: String): String = {
    labelCount = labelCount + 1
    s"$labelTag${usage}_${labelCount.toString}"
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
    case BlockStmt(stmts) =>
      //TODO: run environment building
      stmts flatMap { stmt =>
        assemble(stmt)
      }

    case DeclStmt(decl, assignment) =>
      //TODO: handle declarations
      assert(assertion = false, "TODO")
      List("error")

    case ExprStmt(expr) =>
      assemble(expr)

    case ReturnStmt(expr) =>
      expr match {
        case Some(value) =>
          assemble(value) :::
          leave() ::
          return_() :: Nil
        case None =>
          leave() ::
          return_() :: Nil
      }

    case IfStmt(condition, thenStmt, elseStmt) =>
      val elseLabel = makeLabel("if")
      val commonCode =
        assemble(condition) :::
        jumpIfRegIsFalse(eax, elseLabel) :::
        assemble(thenStmt)

      elseStmt match {
        case Some(elseCode: Stmt) =>
          val endLabel = makeLabel("fi")
          commonCode :::
          jump(endLabel) ::
          prependLabel(elseLabel, assemble(elseCode)) :::
          placeLabel(endLabel, s"end if of if $elseLabel") :: Nil
        case None =>
          commonCode :::
          placeLabel(elseLabel, s"end if of if $elseLabel") :: Nil
      }

    case ForStmt(init, condition, update, bodyStmt) =>
      val startLabel = makeLabel("for")
      val endLabel = makeLabel("end")
      val initCode = init match {
        case Some(initStmt) =>
          assemble(initStmt)
        case None =>
          s"\t ; for loop $startLabel has no init" :: Nil
      }
      def conditionCode() = condition match {
        case Some(conditionExpr) =>
          assemble(conditionExpr)
        case None =>
          s"\t ; for loop $startLabel has no condition" :: Nil
      }
      val updateCode = update match {
        case Some(updateStmt) =>
          assemble(updateStmt)
        case None =>
          s"\t ; for loop $startLabel has no condition" :: Nil
      }
      initCode :::
      conditionCode() :::
      jumpIfRegIsFalse(eax, endLabel) :::
      prependLabel(startLabel, assemble(bodyStmt)) :::
      updateCode :::
      conditionCode() :::
      jumpIfRegIsTrue(eax, startLabel) :::
      placeLabel(endLabel, s"end of for loop $startLabel") :: Nil

    case WhileStmt(condition, bodyStmt) =>
      val startLabel = makeLabel("do")
      val endLabel = makeLabel("end")
      assemble(condition) :::
      jumpIfRegIsFalse(eax, endLabel) :::
      prependLabel(startLabel, assemble(bodyStmt)) :::
      assemble(condition) :::
      jumpIfRegIsTrue(eax, startLabel) :::
      placeLabel(endLabel, s"end of while loop $startLabel") :: Nil
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
    def evaluate(): List[String] =
      assemble(be.lhs) :::
      push(eax) ::
      assemble(be.rhs) :::
      pop(ebx) :: Nil

    def evaluateAndCompare(): List[String] =
      evaluate() :::
      compare(eax, ebx) :: Nil

    be.operatorTok match {
      case Becomes(_, _, _) =>
        assemble(be.rhs) :::
        move(variableStackLocation(be.lhs.asInstanceOf[DeclRefExpr]), eax) :: Nil
      case GT(_, _, _) =>
        evaluateAndCompare() :::
        setOnGreater(al) ::
        moveZeroExtended(eax, al) :: Nil
      case LT(_, _, _) =>
        evaluateAndCompare() :::
        setOnLess(al) ::
        moveZeroExtended(eax, al) :: Nil
      case EQ(_, _, _) =>
        evaluateAndCompare() :::
        setOnEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case GE(_, _, _) =>
        evaluateAndCompare() :::
        setOnGreaterOrEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case LE(_, _, _) =>
        evaluateAndCompare() :::
        setOnLessOrEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case NE(_, _, _) =>
        evaluateAndCompare() :::
        setOnNotEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case Bang(_, _, _) =>
        assert(assertion = false, "Bininary bang is not a thing"); List("error")
      case AmpAmp(_, _, _) =>
        val endLabel = makeLabel("and")
        assemble(be.lhs) :::
        jumpIfRegIsFalse(eax, endLabel) :::
        assemble(be.rhs) :::
        placeLabel(endLabel, s"end of logical and(&&) $endLabel") :: Nil
      case Amp(_, _, _) =>
        evaluate() :::
        binaryAnd(eax, ebx) :: Nil
      case BarBar(_, _, _) =>
        val endLabel = makeLabel("and")
        assemble(be.lhs) :::
        jumpIfRegIsTrue(eax, endLabel) :::
        assemble(be.rhs) :::
        placeLabel(endLabel, s"end of logical or(||) $endLabel") :: Nil
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
  def assemble(ue: UnaryExpr)(implicit localEnv: List[String]): List[String] = ue.operatorTok match {
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

  def assemble(ve: ValExpr): List[String] = ve.value match {
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