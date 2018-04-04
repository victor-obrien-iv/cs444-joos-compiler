package Assembler

import AST._
import Disambiguator.TypeChecker
import Error.Error
import Token._
import i386._

class Assembler(cu: CompilationUnit, typeChecker: TypeChecker) {
  import LabelFactory._

  val labelFactory = new LabelFactory(cu.typeDecl)
  val layout = new Layout(cu.typeDecl, typeChecker)

  def pushParams(params: List[Expr])(implicit st: StackTracker): List[String] =
    params flatMap { param =>
      assemble(param) :::
      push(eax) + comment(s"pushing parameter $param") :: Nil
    }

  def assemble(): List[String] = {
    val typeDecl = cu.typeDecl
    val classLabel = labelFactory.makeClassLabel(typeDecl)

    val (fields, methods, ctors) = typeChecker.partitionMembers(typeDecl.members)
    val staticFields = fields.filter(_.modifiers.exists(_.isInstanceOf[JavaStatic]))
    val staticFieldAsm = assemble(staticFields)
    val methodAsm = assemble(methods)
    val ctorAsm = assemble(ctors)

    val vtableAsm = makeVtable(typeDecl)

    placeLabel(classLabel) ::
      "SECTION .data" ::
      staticFieldAsm :::
      "SECTION .text" ::
      methodAsm :::
      ctorAsm :::
      vtableAsm
  }

  def makeVtable(typeDecl: TypeDecl): List[String] = {
    val vtableLabel = labelFactory.makeVtableLabel(typeDecl)
    val allMethods = typeChecker.findAllInstanceMethods(typeDecl)
    val methodTableEntries = allMethods.map {
      case (typeFrom, method) =>
        placeValue(labelFactory.makeLabel(typeFrom, method))
    }
    placeLabel(vtableLabel)::
      placeValue(typeChecker.findTypeIndex(typeDecl)) ::
      methodTableEntries
  }

  def methodOffset(typeDecl: TypeDecl, methodDecl: MethodDecl): Int = {
    val subTypeOffset = 1
    typeDecl match {
      case InterfaceDecl(modifiers, name, id, extensionOf, members, packageName) =>
        val interfaceMethods = typeChecker.environment.interfaceMethods.map(_._2)
        (typeChecker.findMethodIndex(methodDecl, interfaceMethods) + subTypeOffset)*4
      case ClassDecl(modifiers, name, id, extensionOf, implementationOf, members, packageName) =>
        val vtableMethods = typeChecker.findAllInstanceMethods(typeDecl).map(_._2)
        val methodIndex = typeChecker.findMethodIndex(methodDecl, vtableMethods)
        (methodIndex + typeChecker.interfaceMethodOffset + subTypeOffset)*4
    }
  }

  def assemble(members: List[MemberDecl]): List[String] = {
    members.foldRight(List.empty[String]){
      case (member, asm) =>
        assemble(member) ::: asm
    }
  }

  def assemble(decl: MemberDecl): List[String] = decl match {
    case c: ConstructorDecl => assemble(c)
    case f: FieldDecl=> assemble(f)
    case m: MethodDecl => assemble(m)
  }

  def assemble(fieldDecl: FieldDecl): List[String] ={

    val label = labelFactory.makeLabel(cu.typeDecl, fieldDecl)
    val defaultValue = placeValue(0)

    placeLabel(label) :: defaultValue :: Nil
  }

  def assemble(cd: ConstructorDecl): List[String] = {
    val label = labelFactory.makeLabel(cu.typeDecl, cd)
    val totalLocalBytes = new VarDeclCounter().getNumVarDecl(cd.body)
    implicit val st: StackTracker = new StackTracker(cd.parameters, inObject = true)

    typeChecker.getSuperClass(cu.typeDecl) match {
      case Some(superClass) =>
        val superCtor = typeChecker.findConstructor(Nil, superClass)
        superCtor match {
          case Some(ctor: ConstructorDecl) =>
            val superCtorLabel = labelFactory.makeLabel(superClass, ctor)
            functionEntrance(label, totalLocalBytes) :::
            loadEffectiveAddress(eax, labelFactory.makeVtableLabel(cu.typeDecl)) ::
            move(stackMemory(st.lookUpThis()), eax) + comment("put the vtable ptr at this(0)") ::
            push(eax) ::
            call(superCtorLabel) + comment("call the super ctor") ::
            add(esp, Immediate(4)) ::
            assemble(cd.body) :::
            move(eax, stackMemory(st.lookUpThis())) ::
            functionExit()

          case None =>
            assert(assertion = false, "Super class must have zero param ctor"); throw Error.undefinedMatch
        }
      case None =>
        functionEntrance(label, totalLocalBytes) :::
        assemble(cd.body) :::
        functionExit()

    }
  }

  def assemble(md: MethodDecl): List[String] = md.body match {
    case Some(blockStmt) =>
      val label = labelFactory.makeLabel(cu.typeDecl, md)
      val totalLocalBytes = new VarDeclCounter().getNumVarDecl(blockStmt)
      val isStatic = md.modifiers.exists(_.isInstanceOf[JavaStatic])
      implicit val st: StackTracker = new StackTracker(md.parameters, inObject = !isStatic)
      if(md.name.lexeme == "test" && isStatic)
        //TODO: _start needs to be relocated and actually initialize things
        placeLabel(labelFactory.makeStartLabel()) ::
        call(label) ::
        debugExit() ::
        functionEntrance(label, totalLocalBytes) :::
        assemble(blockStmt) :::
        functionExit()

      else
        functionEntrance(label, totalLocalBytes) :::
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
          push(Immediate(0)) :: Nil
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
        val (methodClass, methodDecl) = typeChecker.declCache.get(ce)
        val isStatic = methodDecl.modifiers.exists(_.isInstanceOf[JavaStatic])
        ce.obj match {
          case Some(objExpr) =>
            val offset = methodOffset(methodClass, methodDecl.asInstanceOf[MethodDecl])
            assemble(objExpr) :::
            pushParams(ce.params) :::
            move(eax, Memory(eax, 0)) ::
            move(eax, Memory(eax, offset)) ::
            call(labelFactory.makeLabel(methodClass, methodDecl)) ::
            discardArgs(ce.params.size + 1) + comment(s"discard args for ${ce.call.lexeme}") :: Nil

          case None =>
            if (isStatic)
              pushParams(ce.params) :::
              call(labelFactory.makeLabel(methodClass, methodDecl)) ::
              discardArgs(ce.params.size) + comment(s"discard args for ${ce.call.lexeme}") :: Nil

            else
              push(stackMemory(st.lookUpThis())) ::
              pushParams(ce.params) :::
              call(labelFactory.makeLabel(methodClass, methodDecl)) ::
              discardArgs(ce.params.size + 1) + comment(s"discard args for ${ce.call.lexeme}") :: Nil

        }
      case _: ThisExpr =>
        move(eax, stackMemory(st.lookUpThis())) :: Nil
      case ce: CastExpr => ???
      case ae: AccessExpr =>
        assemble(ae)
      case aae: ArrayAccessExpr =>
        assemble(aae)
      case ve: ValExpr =>
        assemble(ve)

      case dre: DeclRefExpr =>
        if (typeChecker.declCache.containsKey(dre)) {
          val (typeDecl, memberDecl) = typeChecker.declCache.get(dre)
          memberDecl match {
            case fd: FieldDecl =>
              if (fd.modifiers.exists(_.isInstanceOf[JavaStatic]))
                // static variable to be found in data section
                move(eax, Data(labelFactory.makeLabel(typeDecl, fd))) :: Nil

              else {
                // member variable to be found in object layout
                assert(typeDecl == cu.typeDecl, "DeclRef to different obj?")
                comment(s"load ${fd.name} from ${typeDecl.name}") ::
                loadFromObject(stackMemory(st.lookUpThis()), layout.objectLayout(fd))

              }
            case _: MethodDecl | _: ConstructorDecl =>
              assert(assertion = false, "DeclRef mapped to method?"); throw Error.undefinedMatch
          }
        }
        else move(eax, stackMemory(st.lookUpLocation(dre.reference))) :: Nil

      case ioe: InstanceOfExpr => ???
      case ne: NewExpr =>
        ne match {
          case ObjNewExpr(ctor, params) =>
            val (ctorClass, ctorDecl) = typeChecker.declCache.get(ne)
            comment(s"allocate memory for obj of class ${ctorClass.name}")
            allocate(layout.getObjByteSize(ctorClass)) :::
            push(eax) + comment("push allocated memory as a parameter") ::
            pushParams(params) :::
            call(labelFactory.makeLabel(ctorClass, ctorDecl)) + comment(s"calling ctor ${ctorDecl.name}") ::
            discardArgs(params.size) + comment(s"discard args for ${ctor.name}") :: Nil

          case ArrayNewExpr(arrayType) =>
            val arrayLength = arrayType.size.map(assemble)
            val lengthExpr = arrayLength match {
              case Some(value) => value
              case None => List(move(eax, Immediate(1)))
            }
            val arrayOffset = 8
            val arrayBytes = add(eax, Immediate(arrayOffset))
            val objectDecl = typeChecker.environment.findQualifiedTypeDecl("java.lang.Object").getOrElse(throw Error.langLibraryNotLoaded)

            comment("Finds the length of the array: default 1") :: lengthExpr :::
            comment("Aligns with 4 bytes"):: signedMultiply(eax, Immediate(4)) ::
            comment("Determines how many bytes the array needs") :: arrayBytes ::
            push(eax) ::
            comment("Allocates memory to array") :: call(mallocLabel) ::
            pop(ebx) ::
            comment("Load object vptr") :: loadEffectiveAddress(edx, labelFactory.makeVtableLabel(objectDecl)) ::
            comment("Set the vptr of array to Object") :: move(Memory(eax, 0), edx) ::
            comment("Sets the length of array") :: move(Memory(eax, 4), ebx) ::  Nil
        }
      case ne: NamedExpr =>
        typeChecker.namedExprDeclCache.get(ne) flatMap { tdd =>
          val (typeDecl, decl) = tdd
          loadValue(typeDecl, decl)
        }
    }
  }

  def loadValue(td: TypeDecl, d: Decl)(implicit st: StackTracker): List[String] = d match {
    case ParameterDecl(_, name) =>
      move(eax, stackMemory(st.lookUpLocation(name))) + comment(s"load parameter ${name.lexeme}") :: Nil

    case VarDecl(_, name) =>
      move(eax, stackMemory(st.lookUpLocation(name))) + comment(s"load variable ${name.lexeme}") :: Nil

    case md: MemberDecl => md match {
      case fd: FieldDecl =>
        if (fd.modifiers.exists(_.isInstanceOf[JavaStatic]))
        // static variable to be found in data section
          move(eax, Data(labelFactory.makeLabel(td, fd))) + comment(s"load static field ${fd.name}") :: Nil

        else
        // member variable to be found in object layout
          comment(s"load ${fd.name} from ${td.name}") ::
            loadFromObject(stackMemory(st.lookUpThis()), layout.objectLayout(fd))

      case _: MethodDecl | _: ConstructorDecl =>
        assert(assertion = false, "named expr should not point to function"); throw Error.undefinedMatch
    }
    case td: TypeDecl => td match {
      case _: InterfaceDecl =>
        assert(assertion = false, "named expr pointing to interface?"); throw Error.undefinedMatch
      case _: ClassDecl =>
        assert(assertion = false, "this should be handled by call or instanceof"); throw Error.undefinedMatch
    }
  }

  //TODO fix named expression
  def assemble(accessExpr: AccessExpr)(implicit st: StackTracker): List[String] = accessExpr match {
    case AccessExpr(lhs, field) => //LHS should be a reference
      assemble(lhs)
  }

  def assemble(arrayAccessExpr: ArrayAccessExpr)(implicit st: StackTracker): List[String] = arrayAccessExpr match {
    case ArrayAccessExpr(lhs, index) => //LHS should contain an array expression
      comment("Loads address of array") :: assemble(lhs) :::
      push(eax) ::
      comment("Loads index into array") :: assemble(index) :::
      comment("Loads address of array pushed earlier") :: pop(ebx) ::
      comment("Adds offset since first entry of array should be after vtable and length") :: add(eax, Immediate(2)) ::
      comment("Aligns index") :: signedMultiply(eax, Immediate(4)) ::
      comment("Adds the index offset to the address") :: add(eax, ebx) ::  Nil

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
        def staticStore(td: TypeDecl, fd: FieldDecl) =
          comment("load the rhs value into eax") ::
            assemble(be.rhs) :::
            move(Data(labelFactory.makeLabel(td, fd)), eax) :: Nil

        val leftNamedExpr = be.lhs.asInstanceOf[NamedExpr]
        val decls = typeChecker.namedExprDeclCache.get(leftNamedExpr)
        assert(decls.nonEmpty, "namedExprDeclCache returned null")
        if (leftNamedExpr.name.qualifiers.isEmpty) {
          val (typeDecl, decl) = decls.head
          assert(typeDecl == cu.typeDecl, "local with different class?")
          decl match {
            case fd: FieldDecl =>
              if (fd.modifiers.exists(_.isInstanceOf[JavaStatic]))
                // static variable to be found in data section
                staticStore(typeDecl, fd)

              else
                // member variable to be found in object layout
                comment("load the rhs value into eax") ::
                assemble(be.rhs) :::
                move(edx, stackMemory(st.lookUpThis())) + comment("load this() into edx") ::
                storeIntoObject(edx, layout.objectLayout(fd), eax) +
                  comment(s"store into ${fd.name} in ${typeDecl.name}") :: Nil

            case vd: VarDecl =>
              // the lhs variable is on the stack
              val stackLoc = st.lookUpLocation(vd.name)
              comment("load the rhs value into eax") ::
              assemble(be.rhs) :::
              move(stackMemory(stackLoc), eax) + comment(s"store into local variable ${vd.name.lexeme}") :: Nil

            case d: Decl =>
              assert(assertion = false, s"single name mapped to unexpected Decl $d"); throw Error.undefinedMatch
          }
        }
        else {
          val prologue = decls.dropRight(1) flatMap { tdd =>
            val (typeDecl, decl) = tdd
            loadValue(typeDecl, decl)
          }
          val (typeDecl, decl) = decls.last
          decl match {
            case fd: FieldDecl =>
              if (fd.modifiers.exists(_.isInstanceOf[JavaStatic]))
                // static variable to be found in data section
                staticStore(typeDecl, fd)

              else
                // member variable to be found in object layout
                comment("load the rhs value into eax") ::
                assemble(be.rhs) :::
                move(ecx, eax) + comment("save rhs into ecx") ::
                comment("get the object into eax") ::
                prologue :::
                storeIntoObject(eax, layout.objectLayout(fd), ecx) +
                  comment(s"store into ${fd.name} in ${typeDecl.name} through ecx") :: Nil

            case d: Decl =>
              assert(assertion = false, s"namedExpr mapped to unexpected Decl $d"); throw Error.undefinedMatch
          }
        }
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
          move(eax, Immediate(Int.MinValue)) :: Nil
      }
    case Bang(_, _, _) =>
      assemble(ue.rhs) :::
      compare(eax, Immediate(0)) ::
      setOnEqual(al) ::
      moveZeroExtended(eax, al) :: Nil
  }

  def assemble(ve: ValExpr): List[String] = ve.value match {
    case IntegerLiteral(_, _, _, value) =>
      if (value.isValidInt)
        move(eax, Immediate(value.intValue())) :: Nil
      else {
        assert(value.intValue() == Int.MinValue, "Oversized int is not intmin?")
        throw IntMin() // prior weeding has asserted that a unary minus comes directly before this
      }

    case BooleanLiteral(_, _, value) =>
      if (value)
        move(eax, Immediate(1)) :: Nil
      else
        move(eax, Immediate(0)) :: Nil

    case CharacterLiteral(_, _, _, value) =>
      move(eax, Immediate(value.toInt)) :: Nil

    case StringLiteral(_, _, _, value) =>
      val arrayOffset = 8
      val charArray = value.toCharArray.zipWithIndex.map {
        case (c, i) =>
          move(Memory(eax, i*4 + arrayOffset), Immediate(c))
      }
      val stringDecl = typeChecker.environment.findQualifiedTypeDecl("java.lang.String").getOrElse(throw Error.langLibraryNotLoaded)
      val charType = ArrayType(PrimitiveType(JavaChar(row = 0, col = 0)), None)
      val stringCtor = typeChecker.findConstructor(List(charType), stringDecl).getOrElse(throw Error.langLibraryNotLoaded)
      allocate((value.length + 2) * 4) ::: comment("Allocate bytes for String literal") ::
      charArray.toList ::: comment("Allocated characters: " + value.toCharArray) ::
      push(eax) :: comment("Contstruct String") ::
      call(labelFactory.makeLabel(stringDecl, stringCtor)) :: Nil


    case NullLiteral(_, _, _, _) =>
      move(eax, Immediate(0)) :: Nil
  }

  def assembleSubtypeTable: List[String] = {
    val subTypes = typeChecker.createSubTypeTable
    subTypes.zipWithIndex.flatMap {
      case (types, i) =>
        val label = LabelFactory.makeSubTypeTableEntryLabel(i)
        s"global ${label.name}" ::
        placeLabel(label) ::
        types.map {
          value =>
            if (value) placeValue(1) else placeValue(0)
        }
    }
  }

}
