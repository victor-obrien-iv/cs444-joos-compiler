package Assembler

import AST._
import Token._
import i386._


class Assembler(cu: CompilationUnit) {
  import LabelFactory._
  private val labelFactory = new LabelFactory(cu)

  def assemble(cd: ConstructorDecl): List[String] = {

    val label = labelFactory.makeCtorLabel(cd)
    val paramTotalBytes = (cd.parameters.size + 1) * wordSize
    implicit val st: StackTracker = new StackTracker(cd.parameters, true)

    functionEntrance(label, paramTotalBytes) :::
      // TODO: call super class constructor
    assemble(cd.body) :::
    functionExit()

  }

  def assemble(md: MethodDecl): List[String] = md.body match {
    case Some(blockStmt) =>
      val label = labelFactory.makeMethodLabel(md)
      val paramTotalBytes = md.parameters.size * wordSize
      implicit val st: StackTracker = new StackTracker(md.parameters, false)
      functionEntrance(label, paramTotalBytes) :::
      assemble(blockStmt) :::
      functionExit()

    case None =>
      comment(s"${md.name.lexeme} method declaration") :: Nil
  }


  def assemble(stmt: Stmt)(implicit st: StackTracker): List[String] = stmt match {
    case BlockStmt(stmts) =>
      //TODO: run environment building here?
      stmts flatMap { stmt =>
        assemble(stmt)(new StackTracker(st))
      }

    case DeclStmt(decl, assignment) =>
      st.pushVar(decl)
      assignment match {
        case Some(expr) =>
          assemble(expr) :::
          push(eax) :: Nil
        case None =>
          // no assignment defaults to zero/null
          push(constant(0)) :: Nil
      }

    case ExprStmt(expr) =>
      assemble(expr)

    case ReturnStmt(expr) =>
      expr match {
        case Some(value) =>
          assemble(value) :::
          functionExit()
        case None =>
          functionExit()
      }

    case IfStmt(condition, thenStmt, elseStmt) =>
      val labels = makeLocalLabels("else" :: "if~" :: Nil)
      val elseLabel = labels.head
      val endLabel = labels(1)

      elseStmt match {
        case Some(elseCode: Stmt) =>
          assemble(condition) :::
          jumpIfRegIsFalse(eax, elseLabel) :::
          assemble(thenStmt) :::
          jump(endLabel) ::
          placeLabel(elseLabel) ::
          assemble(elseCode) :::
          placeLabel(endLabel) :: Nil

        case None =>
          assemble(condition) :::
          jumpIfRegIsFalse(eax, endLabel) :::
          assemble(thenStmt) :::
          placeLabel(elseLabel) :: Nil
      }

    case ForStmt(init, condition, update, bodyStmt) =>
      val labels = makeLocalLabels("for" :: "for~" :: Nil)
      val startLabel = labels.head
      val endLabel = labels(1)
      val initCode = init match {
        case Some(initStmt) =>
          assemble(initStmt)
        case None =>
          comment(s"for loop $startLabel has no init") :: Nil
      }
      def conditionCode() = condition match {
        case Some(conditionExpr) =>
          assemble(conditionExpr)
        case None =>
          comment(s"for loop $startLabel has no condition") :: Nil
      }
      val updateCode = update match {
        case Some(updateStmt) =>
          assemble(updateStmt)
        case None =>
          comment(s"for loop $startLabel has no condition") :: Nil
      }
      initCode :::
      conditionCode() :::
      jumpIfRegIsFalse(eax, endLabel) :::
      placeLabel(startLabel) ::
      assemble(bodyStmt) :::
      updateCode :::
      conditionCode() :::
      jumpIfRegIsTrue(eax, startLabel) :::
      placeLabel(endLabel) :: Nil

    case WhileStmt(condition, bodyStmt) =>
      val labels = makeLocalLabels("while" :: "while~" :: Nil)
      val startLabel = labels.head
      val endLabel = labels(1)
      assemble(condition) :::
      jumpIfRegIsFalse(eax, endLabel) :::
      placeLabel(startLabel) ::
      assemble(bodyStmt) :::
      assemble(condition) :::
      jumpIfRegIsTrue(eax, startLabel) :::
      placeLabel(endLabel) :: Nil
  }



  def assemble(expr: Expr)(implicit st: StackTracker): List[String] = {
    def pushParams(params: List[Expr])(implicit st: StackTracker): List[String] = {
      assemble(params.head) :::
      push(eax) ::
      pushParams(params.tail)
    }

    expr match {
      case be: BinaryExpr =>
        assemble(be)
      case ue: UnaryExpr =>
        assemble(ue)
      case pe: ParenExpr =>
        assemble(pe.expr)
      case ce: CallExpr => ???
      case _: ThisExpr => ???
        val thisStackLoc = st.lookUpThis()
        move(eax, stackAddress(thisStackLoc)) :: Nil
      case ce: CastExpr => ???
      case ae: AccessExpr => ???
      case aae: ArrayAccessExpr => ???
      case ve: ValExpr => ???
        assemble(ve)

      case DeclRefExpr(identifier) =>
        val stackLoc = st.lookUpLocation(identifier)
        move(eax, stackAddress(stackLoc)) :: Nil
      case ioe: InstanceOfExpr => ???
      case ne: NewExpr =>
        ne match {
          case ObjNewExpr(ctor, params) =>
            //TODO: environment look up to see what decl ctor refers to
            //TODO: type checking look up to infer param types to choose correct ctor
            allocate(8 /*TODO: actually get the class size*/) :::
            push(eax) ::
            pushParams(params) :::
            call(Label("foobar" /*TODO: actually get the right label*/)) :: Nil
          case ArrayNewExpr(arrayType) =>
            //TODO: not sure what this will look like atm
            ???
        }
      case ne: NamedExpr => ???
    }
  }

  def assemble(be: BinaryExpr)(implicit st: StackTracker): List[String] = {
    def evaluate(): List[String] =
      assemble(be.lhs) :::
      push(eax) ::
      assemble(be.rhs) :::
      pop(ebx) :: Nil

    def evaluateAndCompare(): List[String] =
      evaluate() :::
      compare(eax, ebx) :: Nil

    be.operatorTok match {
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
        signedDivide(ebx)
      case Percent(_, _, _) =>
        evaluate() :::
        signedModulo(ebx)
      case GT(_, _, _) =>
        evaluateAndCompare() :::
        setOnGreater(al) ::
        moveZeroExtended(eax, al) :: Nil
      case LT(_, _, _) =>
        evaluateAndCompare() :::
        setOnLess(al) ::
        moveZeroExtended(eax, al) :: Nil
      case GE(_, _, _) =>
        evaluateAndCompare() :::
        setOnGreaterOrEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case LE(_, _, _) =>
        evaluateAndCompare() :::
        setOnLessOrEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case EQ(_, _, _) =>
        evaluateAndCompare() :::
        setOnEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case NE(_, _, _) =>
        evaluateAndCompare() :::
        setOnNotEqual(al) ::
        moveZeroExtended(eax, al) :: Nil
      case AmpAmp(_, _, _) =>
        val endLabel = makeLocalLabel("and")
        assemble(be.lhs) :::
        jumpIfRegIsFalse(eax, endLabel) :::
        assemble(be.rhs) :::
        placeLabel(endLabel) :: Nil
      case BarBar(_, _, _) =>
        val endLabel = makeLocalLabel("or")
        assemble(be.lhs) :::
        jumpIfRegIsTrue(eax, endLabel) :::
        assemble(be.rhs) :::
        placeLabel(endLabel) :: Nil
      case Amp(_, _, _) =>
        evaluate() :::
        binaryAnd(eax, ebx) :: Nil
      case Bar(_, _, _) =>
        evaluate() :::
        binaryOr(eax, ebx) :: Nil
      case Becomes(_, _, _) =>
        //TODO: environment look up to see what the lhs actually refers to
        // for now just assume its on the stack
        val stackLoc = st.lookUpLocation(be.lhs.asInstanceOf[DeclRefExpr].reference)
        assemble(be.rhs) :::
        move(stackAddress(stackLoc), eax) :: Nil
      case JavaInstanceof(_, _, _) =>
        //TODO: implement instanceof
        ???
    }
  }

  case class IntMin() extends Exception
  def assemble(ue: UnaryExpr)(implicit st: StackTracker): List[String] = ue.operatorTok match {
    case Minus(_, _ ,_) =>
      try
        assemble(ue.rhs) :::
        negate(eax) :: Nil
      catch {
        case IntMin() =>
          move(eax, constant(Int.MinValue)) :: Nil
      }
    case Bang(_, _, _) =>
      assemble(ue.rhs) :::
      compare(eax, constant(0)) ::
      setOnEqual(al) ::
      moveZeroExtended(eax, al) :: Nil
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
      ???
    case NullLiteral(_, _, _, _) =>
      move(eax, constant(0)) :: Nil
  }

}