package Assembler

import AST._
import Environment.{Environment, EnvironmentBuilder}
import Disambiguator.TypeChecker
import Error.Error
import Token._
import i386._



class Assembler(cu: CompilationUnit, tc: TypeChecker) {
  import LabelFactory._
  val labelFactory = new LabelFactory(cu.typeDecl)

  //TODO: move this to where the layout code is
  /**
    * 4 bytes for class ptr
    * 4 bytes for superclass object pointer
    * 4 bytes for each non static field
    */
  def getObjByteSize(objType: TypeDecl): Int = {
    val numNonStaticField = {
      val (fields, _, _) = tc.partitionMembers(objType.members)
      fields.size - fields.count(_.modifiers.exists(_.isInstanceOf[JavaStatic]))
    }
    4 + 4 + numNonStaticField * 4
  }

  def pushParams(params: List[Expr])(implicit st: StackTracker): List[String] =
    params flatMap { param =>
      assemble(param) :::
      push(eax) :: Nil
    }

  def assemble(): List[String] = {
    val typeDecl = cu.typeDecl
    val classLabel = labelFactory.makeClassLabel

    val (fields, methods, ctors) = tc.partitionMembers(typeDecl.members)
    val staticFields = fields.filter(_.modifiers.exists(_.isInstanceOf[JavaStatic]))
    val staticFieldAsm = "SECTION .data" :: assemble(fields)
    val methodAsm = "SECTION .text" :: assemble(methods)
    val ctorAsm = assemble(ctors)

    val vtableAsm = makeVtable(typeDecl)

    placeLabel(classLabel) :: staticFieldAsm ::: methodAsm ::: ctorAsm ::: vtableAsm
  }

  def makeVtable(typeDecl: TypeDecl): List[String] = {
    val vtableLabel = labelFactory.makeVtableLabel
    val allMethods = tc.findAllInstanceMethods(typeDecl)
    val methodTableEntries = allMethods.map {
      case (typeFrom, method) =>
        placeValue(labelFactory.makeLabel(typeFrom, method).name)
    }
    placeLabel(vtableLabel) :: methodTableEntries
  }

  def assemble(members: List[MemberDecl]): List[String] = {
    members.foldRight(List.empty[String]){
      case (field, asm) =>
        assemble(field) ::: asm
    }
  }

  def assemble(decl: MemberDecl): List[String] = decl match {
    case c: ConstructorDecl => assemble(c)
    case f: FieldDecl=> assemble(f)
    case m: MethodDecl => assemble(m)
  }

  def assemble(fieldDecl: FieldDecl): List[String] ={

    val label = labelFactory.makeLabel(cu.typeDecl, fieldDecl)
    val defaultValue = placeValue("0")

    placeLabel(label) :: defaultValue :: Nil
  }

  def assemble(cd: ConstructorDecl): List[String] = {
    val label = labelFactory.makeLabel(cu.typeDecl, cd)
    val paramTotalBytes = (cd.parameters.size + 1) * wordSize
    implicit val st: StackTracker = new StackTracker(cd.parameters, inObject = true)

    tc.getSuperClass(cu.typeDecl) match {
      case Some(superClass) =>
        val superCtor = tc.findConstructor(Nil, superClass)
        val superObjSize = getObjByteSize(superClass)
        superCtor match {
          case Some(ctor: ConstructorDecl) =>
            val superCtorLabel = labelFactory.makeLabel(superClass, ctor)
            functionEntrance(label, paramTotalBytes) :::
            allocate(superObjSize) :::
            push(eax) ::
            call(superCtorLabel) ::
            assemble(cd.body) :::
            move(eax, stackAddress(st.lookUpThis())) ::
            functionExit()
          case None =>
            assert(assertion = false, "Super class must have zero param ctor"); throw Error.undefinedMatch
        }
      case None =>
        functionEntrance(label, paramTotalBytes) :::
        assemble(cd.body) :::
        functionExit()
    }


  }

  def assemble(md: MethodDecl): List[String] = md.body match {
    case Some(blockStmt) =>
      val label = labelFactory.makeLabel(cu.typeDecl, md)
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
    expr match {
      case be: BinaryExpr =>
        assemble(be)
      case ue: UnaryExpr =>
        assemble(ue)
      case pe: ParenExpr =>
        assemble(pe.expr)
      case ce: CallExpr =>
        val (methodClass, methodDecl) = tc.declCache(ce)
        val isStatic = methodDecl.modifiers.exists(_.isInstanceOf[JavaStatic])
        ce.obj match {
          case Some(objExpr) => //TODO: this is static dispatch, need to change to dynamic dispatch
            assert(!isStatic, "Static call on object?")
            assemble(objExpr) :::
            push(eax) ::
            pushParams(ce.params) :::
            call(labelFactory.makeLabel(methodClass, methodDecl)) ::
            add(esp, constant(4 * (ce.params.size + 1))) + comment(s"discard args for ${ce.call.lexeme}") :: Nil
          case None =>
            if (isStatic)
              pushParams(ce.params) :::
              call(labelFactory.makeLabel(methodClass, methodDecl)) ::
              add(esp, constant(4 * ce.params.size)) + comment(s"discard args for ${ce.call.lexeme}") :: Nil
            else
              push(stackAddress(st.lookUpThis())) ::
              pushParams(ce.params) :::
              call(labelFactory.makeLabel(methodClass, methodDecl)) ::
              add(esp, constant(4 * (ce.params.size + 1))) + comment(s"discard args for ${ce.call.lexeme}") :: Nil
        }
      case _: ThisExpr =>
        val thisStackLoc = st.lookUpThis()
        move(eax, stackAddress(thisStackLoc)) :: Nil
      case ce: CastExpr => ???
      case ae: AccessExpr => ???
      case aae: ArrayAccessExpr => ???
      case ve: ValExpr =>
        assemble(ve)

      case DeclRefExpr(identifier) =>
        val stackLoc = st.lookUpLocation(identifier)
        move(eax, stackAddress(stackLoc)) :: Nil
      case ioe: InstanceOfExpr => ???
      case ne: NewExpr =>
        ne match {
          case ObjNewExpr(ctor, params) =>
            val (ctorClass, ctorDecl) = tc.declCache(ne)
            allocate(getObjByteSize(ctorClass)) :::
            push(eax) ::
            pushParams(params) :::
            call(labelFactory.makeLabel(ctorClass, ctorDecl)) ::
            add(esp, constant(4 * params.size)) + comment(s"discard args for ${ctor.name}") :: Nil
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