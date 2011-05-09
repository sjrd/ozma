/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.nsc
package backend.mozart

import scala.collection._
import scala.collection.mutable.{ LinkedHashSet, HashSet, HashMap, ListBuffer }

import ozma._

/** This is the compiler component that produces Oz code from AST
 *
 *  @author  Sébastien Doeraene
 *  @version 1.0
 *
 */
abstract class GenOzCode extends OzmaSubComponent {
  import global._
  import ozcodes._

  import definitions.{
    ArrayClass, ObjectClass, ThrowableClass, StringClass, StringModule,
    NothingClass, NullClass, AnyRefClass,
    Object_equals, Object_isInstanceOf, Object_asInstanceOf, ScalaRunTimeModule,
    BoxedNumberClass, BoxedCharacterClass,
    getMember
  }

  val phaseName = "ozcode"

  override def newPhase(p: Phase) = new OzCodePhase(p)

  class OzCodePhase(prev: Phase) extends StdPhase(prev) {
    var unit: CompilationUnit = _
    val unitClasses: LinkedHashSet[OzClass] = new LinkedHashSet

    override def run {
      scalaPrimitives.init
      nativeMethods.init
      super.run
    }

    override def apply(unit: CompilationUnit): Unit = {
      this.unit = unit
      informProgress("Generating Oz code for " + unit)

      unitClasses.clear
      gen(unit.body)

      unit.body = OzCodeClasses(makeClasses(), makeModules())
      this.unit = null
    }

    def gen(tree: Tree): Context = gen(tree, new Context())

    def gen(trees: List[Tree], ctx: Context): Context = {
      var ctx1 = ctx
      for (t <- trees) ctx1 = gen(t, ctx1)
      ctx1
    }

    /////////////////// Utils ///////////////////////

    lazy val SingleAssignmentClass =
      definitions.getClass("scala.ozma.singleAssignment")

    def hasSingleAssignSemantics(sym: Symbol) =
      !sym.isVariable || sym.hasAnnotation(SingleAssignmentClass)

    /////////////////// Code generation ///////////////////////

    def gen(tree: Tree, ctx: Context): Context = tree match {
      case EmptyTree => ctx

      case PackageDef(pid, stats) =>
        gen(stats, ctx setPackage pid.name)

      case ClassDef(mods, name, _ /* tps */, impl) =>
        log("Generating class: " + tree.symbol.fullName)
        val outerClass = ctx.clazz
        ctx setClass (new OzClass(tree.symbol) setCompilationUnit unit)
        addClassFields(ctx, tree.symbol)
        //classes += (tree.symbol -> ctx.clazz)
        unitClasses += ctx.clazz
        gen(impl, ctx)
        ctx.clazz.methods = ctx.clazz.methods.reverse // preserve textual order
        ctx.clazz.fields  = ctx.clazz.fields.reverse  // preserve textual order
        ctx setClass outerClass

      // !! modules should be eliminated by refcheck... or not?
      case ModuleDef(mods, name, impl) =>
        abort("Modules should not reach backend!")

      case ValDef(mods, name, tpt, rhs) =>
        ctx // we use the symbol to add fields

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        if (settings.debug.value)
          log("Entering method " + name)
        val m = new OzMethod(tree.symbol)
        m.sourceFile = unit.source.toString()
        ctx.clazz.addMethod(m)

        var ctx1 = ctx.enterMethod(m, tree.asInstanceOf[DefDef])
        addMethodParams(ctx1, vparamss)
        m.native = m.symbol.hasAnnotation(definitions.NativeAttr)

        ctx1.method.code = if (m.native)
          nativeMethods.getBodyFor(m.symbol)
        else if (!m.isAbstractMethod)
          genExpression(rhs, ctx1)
        else
          null

        ctx1

      case Template(_, _, body) =>
        gen(body, ctx)

      case _ =>
        abort("Illegal tree in gen: " + tree)
    }

    /**
     * Add all fields of the given class symbol to the current OzCode
     * class.
     */
    private def addClassFields(ctx: Context, cls: Symbol) {
      if (settings.debug.value)
        assert(ctx.clazz.symbol eq cls,
               "Classes are not the same: " + ctx.clazz.symbol + ", " + cls)

      /** Non-method term members are fields, except for module members. Module
       *  members can only happen on .NET (no flatten) for inner traits. There,
       *  a module symbol is generated (transformInfo in mixin) which is used
       *  as owner for the members of the implementation class (so that the
       *  backend emits them as static).
       *  No code is needed for this module symbol.
       */
      for (f <- cls.info.decls ; if !f.isMethod && f.isTerm && !f.isModule)
        ctx.clazz addField new OzField(f)
    }

    /**
     * Add parameters to the current OzCode method. It is assumed the methods
     * have been uncurried, so the list of lists contains just one list.
     */
    private def addMethodParams(ctx: Context, vparamss: List[List[ValDef]]) {
      vparamss match {
        case Nil => ()

        case vparams :: Nil =>
          for (p <- vparams) {
            val lv = new Local(p.symbol, true)
            ctx.method.addParam(lv)
          }
          ctx.method.params = ctx.method.params.reverse

        case _ =>
          abort("Malformed parameter list: " + vparamss)
      }
    }

    def genExpression(tree: Tree, ctx: Context): ast.Phrase = {
      if (settings.debug.value)
        log("at line: " + (if (tree.pos.isDefined) tree.pos.line else tree.pos))

      (tree match {
        case LabelDef(name, params, rhs) =>
          abort("Labels should not reach the GenOzCode back-end")

        case ValDef(_, nme.THIS, _, _) =>
          if (settings.debug.value)
            log("skipping trivial assign to _$this: " + tree)
          ast.UnitVal()

        case ValDef(_, _, _, rhs) =>
          val sym = tree.symbol
          val local = ctx.method.addLocal(new Local(sym, false))

          val value = if (sym.hasAnnotation(SingleAssignmentClass)) {
            ast.Wildcard() setPos rhs.pos
          } else if (rhs == EmptyTree) {
            if (settings.debug.value)
              log("Uninitialized variable " + tree + " at: " + (tree.pos));
            ast.Wildcard() setPos tree.pos
          } else {
            genExpression(rhs, ctx)
          }

          val rightOfEqual = if (hasSingleAssignSemantics(sym))
            value
          else
            genBuiltinApply("NewCell", value)

          rightOfEqual match {
            case _: ast.Wildcard => ast.UnitVal()
            case _ =>
              ast.And(ast.Eq(varForSymbol(sym) setPos tree.pos, rightOfEqual),
                  ast.UnitVal())
          }

        case t @ If(cond, thenp, elsep) =>
          ast.IfThenElse(genExpression(cond, ctx), genExpression(thenp, ctx),
              genExpression(elsep, ctx))

        case Return(expr) =>
          abort("Cannot encode a 'return' in Oz")

        case t @ Try(_, _, _) =>
          ast.Atom("TODO: generate a try")

        case Throw(expr) =>
          ast.Raise(genExpression(expr, ctx))

        case New(tpt) =>
          abort("Unexpected New")

        case Apply(TypeApply(fun, targs), _) =>
          val sym = fun.symbol
          val cast = sym match {
            case Object_isInstanceOf => false
            case Object_asInstanceOf => true
            case _ =>
              abort("Unexpected type application " + fun + "[sym: " + sym.fullName + "]" + " in: " + tree)
          }

          val Select(obj, _) = fun
          val from = obj.tpe
          val to = targs.head.tpe
          val l = toTypeKind(from)
          val r = toTypeKind(to)
          val source = genExpression(obj, ctx)

          if (l.isValueType && r.isValueType) {
            if (cast)
              genConversion(l, r, source)
            else
              ast.Bool(l == r)
          }
          else if (l.isValueType) {
            val blackhole = blackholeReturnedValue(source)
            val result = if (cast) {
              ast.Raise(genNew(definitions.ClassCastExceptionClass))
            } else
              ast.False()
            ast.And(blackhole, result)
          }
          else if (r.isValueType && cast) {
            // Erasure should have added an unboxing operation to prevent that.
            assert(false, tree)
            source
          }
          else if (r.isValueType)
            genCast(from, definitions.boxedClass(to.typeSymbol).tpe, source, false)
          else
            genCast(from, to, source, cast)

        // 'super' call
        case Apply(fun @ Select(sup @ Super(_, mix), _), args) =>
          if (settings.debug.value)
            log("Call to super: " + tree)

          val superClass = varForSymbol(sup.symbol.superClass) setPos sup.pos
          val arguments = args map { genExpression(_, ctx) }
          val message = buildMessage(atomForSymbol(fun.symbol) setPos fun.pos,
              arguments)

          ast.ObjApply(superClass, message)

        // 'new' constructor call
        case Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          if (settings.debug.value)
            assert(ctor.isClassConstructor,
                   "'new' call to non-constructor: " + ctor.name)

          val arguments = args map { genExpression(_, ctx) }

          val generatedType = toTypeKind(tpt.tpe)
          if (settings.debug.value)
            assert(generatedType.isReferenceType || generatedType.isArrayType,
                 "Non reference type cannot be instantiated: " + generatedType)

          generatedType match {
            case arr @ ARRAY(elem) =>
              genNewArray(arr.elementKind, arr.dimensions, arguments)

            case rt @ REFERENCE(cls) =>
              genNew(cls, arguments,
                  atomForSymbol(fun.symbol) setPos fun.pos)
          }

        case app @ Apply(fun, args) =>
          val sym = fun.symbol

          if (sym.isLabel) {  // jump to a label
            abort("Cannot jump to a label in Oz")
          } else if (isPrimitive(sym)) {
            // primitive operation
            genPrimitiveOp(app, ctx)
          } else {  // normal method call
            if (settings.debug.value)
              log("Gen CALL_METHOD with sym: " + sym + " isStaticSymbol: " + sym.isStaticMember);

            if (sym == ctx.method.symbol)
              ctx.method.recursive = true

            val Select(receiver, _) = fun
            val instance = genExpression(receiver, ctx)
            val arguments = args map { genExpression(_, ctx) }
            val message = buildMessage(atomForSymbol(fun.symbol) setPos fun.pos,
                arguments)

            ast.Apply(instance, List(message))
          }

        case This(qual) =>
          assert(tree.symbol == ctx.clazz.symbol || tree.symbol.isModuleClass,
                 "Trying to access the this of another class: " +
                 "tree.symbol = " + tree.symbol + ", ctx.clazz.symbol = " + ctx.clazz.symbol + " compilation unit:"+unit)

          if (tree.symbol.isModuleClass && tree.symbol != ctx.clazz.symbol) {
            if (settings.debug.value)
              log("LOAD_MODULE from 'This': " + tree.symbol);
            assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
            genModule(ctx, tree.symbol, tree.pos)
          } else {
            ast.Self()
          }

        case Select(Ident(nme.EMPTY_PACKAGE_NAME), module) =>
          if (settings.debug.value) {
            assert(tree.symbol.isModule,
                   "Selection of non-module from empty package: " + tree.toString() +
                   " sym: " + tree.symbol +
                   " at: " + (tree.pos))
            log("LOAD_MODULE from Select(<emptypackage>): " + tree.symbol)
          }
          assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
          genModule(ctx, tree.symbol, tree.pos)

        case Select(qualifier, selector) =>
          val sym = tree.symbol

          if (sym.isModule) {
            if (settings.debug.value)
              log("LOAD_MODULE from Select(qualifier, selector): " + sym)
            assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
            genModule(ctx, sym, tree.pos)
          } else {
            val symVar = varForSymbol(sym)

            if (qualifier.isInstanceOf[This] || symVar.isInstanceOf[ast.Variable])
              ast.At(symVar)
            else {
              val instance = genExpression(qualifier, ctx)
              val arguments = List(symVar)
              val message = buildMessage(
                  ast.Atom("$getPublic$") setPos tree.pos, arguments)

              ast.Apply(instance, List(message))
            }
          }

        case Ident(name) =>
          val sym = tree.symbol
          if (!sym.isPackage) {
            if (sym.isModule) {
              if (settings.debug.value)
                log("LOAD_MODULE from Ident(name): " + sym)
              assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
              genModule(ctx, sym, tree.pos)
            } else {
              val value = varForSymbol(sym)

              if (hasSingleAssignSemantics(sym))
                value
              else
                ast.At(value)
            }
          } else
            ast.Atom("package")

        case Literal(value) =>
          value.tag match {
            case UnitTag =>
              ast.UnitVal()
            case BooleanTag =>
              ast.Bool(value.booleanValue)
            case ByteTag | ShortTag | CharTag | IntTag | LongTag =>
              ast.IntLiteral(value.longValue)
            case FloatTag | DoubleTag =>
              ast.FloatLiteral(value.doubleValue)
            case StringTag =>
              genBuiltinApply("StringLiteral",
                  ast.StringLiteral(value.stringValue))
            case NullTag =>
              ast.NullVal()
          }

        case Block(stats, expr) =>
          val statements = stats map { genStatement(_, ctx) }
          val expression = genExpression(expr, ctx)

          ast.And((statements ++ List(expression)):_*)

        case Typed(Super(_, _), _) =>
          genExpression(This(ctx.clazz.symbol), ctx)

        case Typed(expr, _) =>
          genExpression(expr, ctx)

        case Assign(lhs @ Select(qualifier, _), rhs) =>
          val sym = lhs.symbol
          val symVar = varForSymbol(sym) setPos lhs.pos

          val assignment =
            if (qualifier.isInstanceOf[This] || symVar.isInstanceOf[ast.Variable])
              ast.ColonEquals(symVar, genExpression(rhs, ctx))
            else {
              val instance = genExpression(qualifier, ctx)
              val arguments = List(symVar, genExpression(rhs, ctx))
              val message = ast.Tuple(
                  ast.Atom("$setPublic$") setPos tree.pos, arguments:_*)

              ast.Apply(instance, List(message))
            }

          ast.And(assignment, ast.UnitVal())

        case Assign(lhs, rhs) =>
          val sym = lhs.symbol
          val assignment = if (hasSingleAssignSemantics(sym))
            ast.Eq(varForSymbol(sym) setPos lhs.pos, genExpression(rhs, ctx))
          else
            ast.ColonEquals(varForSymbol(sym) setPos lhs.pos,
                genExpression(rhs, ctx))
          ast.And(assignment, ast.UnitVal())

        case ArrayValue(tpt @ TypeTree(), _elems) =>
          abort("FIXME: cannot create an array initialize")

        case Match(selector, cases) =>
          if (settings.debug.value)
            log("Generating SWITCH statement.")

          val expr = genExpression(selector, ctx)

          var clauses: List[ast.CaseClause] = Nil
          var elseClause: ast.OptElse = ast.NoElse() setPos tree.pos

          for (caze @ CaseDef(pat, guard, body) <- cases) {
            assert(guard == EmptyTree)

            val bodyAST = genExpression(body, ctx)

            pat match {
              case lit: Literal =>
                clauses = ast.CaseClause(genExpression(lit, ctx),
                    bodyAST) :: clauses
              case Ident(nme.WILDCARD) =>
                elseClause = bodyAST
              case _ =>
                abort("Invalid case statement in switch-like pattern match: " +
                    tree + " at: " + (tree.pos))
            }
          }

          ast.Case(expr, clauses.reverse, elseClause)

        case EmptyTree =>
          ast.UnitVal()

        case _ =>
          abort("Unexpected tree in genLoad: " + tree + "/" + tree.getClass +
              " at: " + tree.pos)
      }) setDefaultPos tree.pos
    }

    private def genStatement(tree: Tree, ctx: Context): ast.Phrase = {
      blackholeReturnedValue(genExpression(tree, ctx))
    }

    private def isPrimitive(sym: Symbol) =
      scalaPrimitives.isPrimitive(sym) && (sym ne definitions.String_+)

    private def genPrimitiveOp(tree: Apply, ctx: Context): ast.Phrase = {
      import scalaPrimitives._

      val sym = tree.symbol
      val Apply(fun @ Select(receiver, _), args) = tree
      val code = scalaPrimitives.getPrimitive(sym, receiver.tpe)

      if (isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code))
        genSimpleOp(tree, receiver :: args, ctx, code)
      else if (code == HASH)
        genScalaHash(tree, receiver, ctx)
      else if (isArrayOp(code))
        genArrayOp(tree, ctx, code)
      else if (isCoercion(code))
        genCoercion(tree, receiver, ctx, code)
      else
        abort("Primitive operation not handled yet: " + sym.fullName + "(" +
            fun.symbol.simpleName + ") " + " at: " + (tree.pos))
    }

    private def genSimpleOp(tree: Apply, args: List[Tree], ctx: Context,
        code: Int): ast.Phrase = {
      import scalaPrimitives._

      val sources = args map (arg => genExpression(arg, ctx))

      sources match {
        // Unary operation
        case List(source) =>
          (code match {
            case POS =>
              source // nothing to do
            case NEG =>
              ast.UnaryOpApply("~", source)
            case NOT =>
              genBuiltinApply("BinNot", source)
            case ZNOT =>
              genBuiltinApply("Not", source)
            case _ =>
              abort("Unknown unary operation code: " + code)
          }) setPos tree.pos

        // Binary operation
        case List(lsrc, rsrc) =>
          def divOperator = toTypeKind(args.head.tpe) match {
            case _:INT => "div"
            case _:FLOAT => "/"
          }

          (code match {
            case ADD => ast.BinaryOpApply("+", lsrc, rsrc)
            case SUB => ast.BinaryOpApply("-", lsrc, rsrc)
            case MUL => ast.BinaryOpApply("*", lsrc, rsrc)
            case DIV => ast.BinaryOpApply(divOperator, lsrc, rsrc)
            case MOD => ast.BinaryOpApply("mod", lsrc, rsrc)
            case OR => genBuiltinApply("BinOr", lsrc, rsrc)
            case XOR => genBuiltinApply("BinXor", lsrc, rsrc)
            case AND => genBuiltinApply("BinAnd", lsrc, rsrc)
            case LSL => genBuiltinApply("LSL", lsrc, rsrc)
            case LSR => genBuiltinApply("LSR", lsrc, rsrc)
            case ASR => genBuiltinApply("ASR", lsrc, rsrc)
            case LT => ast.BinaryOpApply("<", lsrc, rsrc)
            case LE => ast.BinaryOpApply("=<", lsrc, rsrc)
            case GT => ast.BinaryOpApply(">", lsrc, rsrc)
            case GE => ast.BinaryOpApply(">=", lsrc, rsrc)
            case ID | EQ => ast.BinaryOpApply("==", lsrc, rsrc)
            case NI | NE => ast.BinaryOpApply("\\=", lsrc, rsrc)
            case ZOR => ast.OrElse(lsrc, rsrc)
            case ZAND => ast.AndThen(lsrc, rsrc)
            case _ =>
              abort("Unknown binary operation code: " + code)
          }) setPos tree.pos

        case _ =>
          abort("Too many arguments for primitive function: " + tree)
      }
    }

    private def genScalaHash(tree: Apply, receiver: Tree,
        ctx: Context): ast.Phrase = {
      val instance = varForSymbol(ScalaRunTimeModule) setPos tree.pos
      val arguments = List(genExpression(receiver, ctx),
          ast.Dollar() setPos tree.pos)
      val sym = getMember(ScalaRunTimeModule, "hash")
      val message = ast.Tuple(atomForSymbol(sym) setPos tree.pos,
          arguments:_*)

      ast.Apply(instance, List(message)) setPos tree.pos
    }

    private def genArrayOp(tree: Tree, ctx: Context, code: Int) = {
      import scalaPrimitives._

      val Apply(Select(arrayObj, _), args) = tree
      val arrayValue = genExpression(arrayObj, ctx)
      val arguments = args map (arg => genExpression(arg, ctx))

      if (scalaPrimitives.isArrayGet(code)) {
        // get an item of the array
        if (settings.debug.value)
          assert(args.length == 1,
                 "Too many arguments for array get operation: " + tree)

        val message = buildMessage(ast.Atom("get"), arguments)
        ast.Apply(arrayValue, List(message))
      }
      else if (scalaPrimitives.isArraySet(code)) {
        // set an item of the array
        if (settings.debug.value)
          assert(args.length == 2,
                 "Too many arguments for array set operation: " + tree)

        val message = ast.Tuple(ast.Atom("put"), arguments:_*)
        ast.And(ast.Apply(arrayValue, List(message)), ast.UnitVal())
      }
      else {
        // length of the array
        val message = buildMessage(ast.Atom("length"), Nil)
        ast.Apply(arrayValue, List(message))
      }
    }

    private def genCoercion(tree: Apply, receiver: Tree, ctx: Context,
        code: Int): ast.Phrase = {
      import scalaPrimitives._

      val source = genExpression(receiver, ctx)

      (code: @scala.annotation.switch) match {
        case B2F | B2D | S2F | S2D | C2F | C2D | I2F | I2D | L2F | L2D =>
          genBuiltinApply("IntToFloat", source) setPos tree.pos

        case F2B | F2S | F2C | F2I | F2L | D2B | D2S | D2C | D2I | D2L =>
          genBuiltinApply("FloatToInt", source) setPos tree.pos

        case _ => source
      }
    }

    private def blackholeReturnedValue(expr: ast.Phrase): ast.Phrase = {
      expr match {
        case _:ast.Constant => ast.Skip() setPos expr.pos

        case ast.And(statement, _:ast.RecordLabel) => statement

        case ast.And(statements @ _*) =>
          val last :: others = statements.toList.reverse
          val newLast = blackholeReturnedValue(last)
          val newOthers = others filterNot (_.isInstanceOf[ast.Skip])
          val newStatements = (newLast :: newOthers).reverse
          ast.And(newStatements:_*)

        case ast.IfThenElse(condition, thenStats, elseStats: ast.Phrase) =>
          ast.IfThenElse(condition,
              blackholeReturnedValue(thenStats),
              blackholeReturnedValue(elseStats)) setPos expr.pos

        case ast.Raise(_) => expr

        case _ => ast.Eq(ast.Wildcard(), expr) setPos expr.pos
      }
    }

    private def genModule(ctx: Context, sym: Symbol, pos: Position) =
      if (sym.isModuleClass)
        varForSymbol(sym.companionModule) setPos pos
      else
        varForSymbol(sym) setPos pos

    def genConversion(from: TypeKind, to: TypeKind, value: ast.Phrase) = {
      def int0 = ast.IntLiteral(0) setPos value.pos
      def int1 = ast.IntLiteral(1) setPos value.pos
      def float0 = ast.FloatLiteral(0.0) setPos value.pos
      def float1 = ast.FloatLiteral(1.0) setPos value.pos

      (from, to) match {
        case _ if from eq to => value

        case (_:INT, _:FLOAT) => genBuiltinApply("IntToFloat", value)
        case (_:FLOAT, _:INT) => genBuiltinApply("FloatToInt", value)

        case (_:INT, BOOL) => ast.BinaryOpApply("\\=", value, int0)
        case (_:FLOAT, BOOL) => ast.BinaryOpApply("\\=", value, float0)

        case (BOOL, _:INT) => ast.IfThenElse(value, int1, int0)
        case (BOOL, _:FLOAT) => ast.IfThenElse(value, float1, float0)
      }
    }

    def genCast(from: Type, to: Type, value: ast.Phrase, cast: Boolean) = {
      genBuiltinApply(if (cast) "AsInstance" else "IsInstance",
          value, varForClass(to.typeSymbol) setPos value.pos)
    }

    /////////////////// Context ///////////////////////

    class Context {
      /** The current package. */
      var packg: Name = _

      /** The current class. */
      var clazz: OzClass = _

      /** The current method. */
      var method: OzMethod = _

      /** Current method definition. */
      var defdef: DefDef = _

      def this(other: Context) = {
        this()
        this.packg = other.packg
        this.clazz = other.clazz
        this.method = other.method
      }

      override def toString(): String = {
        val buf = new StringBuilder()
        buf.append("\tpackage: ").append(packg).append('\n')
        buf.append("\tclazz: ").append(clazz).append('\n')
        buf.append("\tmethod: ").append(method).append('\n')
        buf.toString()
      }

      def setPackage(p: Name): this.type = {
        this.packg = p
        this
      }

      def setClass(c: OzClass): this.type = {
        this.clazz = c
        this
      }

      def setMethod(m: OzMethod): this.type = {
        this.method = m
        this
      }

      /** Prepare a new context upon entry into a method.
       *
       *  @param m ...
       *  @param d ...
       *  @return  ...
       */
      def enterMethod(m: OzMethod, d: DefDef): Context = {
        val ctx1 = new Context(this) setMethod(m)
        ctx1.defdef = d
        ctx1
      }
    }

    /////////////////// Class assembly ///////////////////////

    def makeClasses(): List[ast.ClassDef] = {
      val result: ListBuffer[ast.ClassDef] = new ListBuffer

      for (clazz <- unitClasses)
        result += makeClass(clazz)

      result.toList
    }

    def makeClass(clazz: OzClass) = {
      ast.ClassDef(varForSymbol(clazz.symbol) setPos clazz.symbol.pos,
          makeClassDescriptors(clazz), makeClassMethods(clazz))
    }

    def makeClassDescriptors(clazz: OzClass): List[ast.ClassDescriptor] = {
      val withAttrs = if (clazz.fields.isEmpty)
        Nil
      else {
        val attrs = ast.Attr(clazz.fields.map {
          field => varForSymbol(field.symbol) setPos field.symbol.pos
        })

        List(attrs)
      }

      val sym = clazz.symbol
      val withFrom = if (sym.isInterface)
        withAttrs
      else {
        val superClass = if (sym.superClass == NoSymbol)
          ObjectClass
        else
          sym.superClass

        val superClassVar = varForSymbol(superClass) setPos sym.pos
        (ast.From(List(superClassVar))) :: withAttrs
      }

      withFrom
    }

    def makeClassMethods(clazz: OzClass): List[ast.MethodDef] = {
      clazz.methods.filter { _.code ne null }
                   .map { method => makeMethodDef(method) }
    }

    def makeMethodDef(method: OzMethod): ast.MethodDef = {
      val name = atomForSymbol(method.symbol) setPos method.symbol.pos
      val args0 = method.params.map {
        local => ast.MethodArg(None,
            varForSymbol(local.sym).asInstanceOf[
              ast.MethodArgName] setPos local.sym.pos, None)
      }
      val args = args0 ++ List(ast.MethodArg(None, ast.Dollar(), None))
      val body = method.code.asInstanceOf[ast.Phrase]

      val locals = method.locals filter {
        local => !method.params.contains(local)
      }

      val actualBody = if (locals.isEmpty)
        body
      else {
        val localVars = locals map { local =>
          varForSymbol(local.sym) setPos local.sym.pos
        }
        ast.LocalDef(ast.And(localVars:_*), body) setPos method.symbol.pos
      }

      ast.MethodDef(name, args, actualBody) setPos method.symbol.pos
    }

    /////////////////// Module assembly ///////////////////////

    def makeModules(): List[ast.Eq] = {
      val classes = unitClasses.toList

      val modules = for (clazz <- classes if (clazz.symbol.isModuleClass))
        yield makeModule(clazz)

      val classConstants = for (clazz <- classes)
        yield makeClassConstant(clazz)

      modules ::: classConstants
    }

    def makeModule(clazz: OzClass) = {
      val sym = clazz.symbol
      val module = sym.companionModule

      val atom = if (sym.isImplClass)
        ast.Atom("<init>#1063877011")
      else
        atomForSymbol(clazz.symbol.primaryConstructor)

      val value = genNew(clazz.symbol, Nil, atom)

      ast.Eq(varForSymbol(module), makeByNeed(value))
    }

    def makeClassConstant(clazz: OzClass) = {
      import definitions._

      val sym = clazz.symbol
      val fullName = ast.StringLiteral(sym.fullName + suffixFor(sym))

      val value = if (isPrimitiveType(sym)) {
        val encodedArrayType = toTypeKind(sym.tpe).toType.typeSymbol match {
          case UnitClass     => "U"
          case BooleanClass  => "Z"
          case CharClass     => "C"
          case ByteClass     => "B"
          case ShortClass    => "S"
          case IntClass      => "I"
          case LongClass     => "J"
          case FloatClass    => "F"
          case DoubleClass   => "D"
          case x => abort("Unknown primitive type: " + x.fullName)
        }

        val message = ast.Tuple(ast.Atom("<init>"),
            fullName, ast.Atom(encodedArrayType), ast.Wildcard())

        genBuiltinApply("NewObject",
            ast.QuotedVar("type:java.lang.Class$PrimitiveClass"),
            varForClass(ClassClass), message)
      } else {
        val superClass = if (sym.superClass == NoSymbol)
          varForClass(ObjectClass)
        else
          varForClass(sym.superClass)
        val mixins = ast.ListLiteral((sym.mixinClasses map varForClass):_*)

        val rawAncestors = if (sym.ancestors.isEmpty)
          List(ObjectClass)
        else
          sym.ancestors
        val ancestors = ast.ListLiteral((rawAncestors map varForClass):_*)

        val arguments = List(fullName, superClass, mixins, ancestors)
        genNew(definitions.ClassClass, arguments, ast.Atom("<init>"))
      }

      ast.Eq(varForClass(clazz.symbol), makeByNeed(value))
    }

    private def isPrimitiveType(sym: Symbol) = toTypeKind(sym.tpe) match {
      case _:REFERENCE => false
      case _:ARRAY => false
      case _ => true
    }

    def makeByNeed(value: ast.Phrase) = {
      val fun = ast.Fun(ast.Dollar(), Nil, value)
      genBuiltinApply("ByNeed", fun)
    }
  }
}
