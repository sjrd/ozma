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

        if (!m.isAbstractMethod && !m.native)
          ctx1.method.code = genExpression(rhs, ctx1)

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

      tree match {
        case LabelDef(name, params, rhs) =>
          abort("Labels should not reach the GenOzCode back-end")

        case ValDef(_, nme.THIS, _, _) =>
          if (settings.debug.value)
            log("skipping trivial assign to _$this: " + tree)
          ast.Atom("unit")

        case ValDef(_, _, _, rhs) =>
          val sym = tree.symbol
          val local = ctx.method.addLocal(new Local(sym, false))

          val value = if (sym.hasAnnotation(SingleAssignmentClass)) {
            ast.Wildcard()
          } else if (rhs == EmptyTree) {
            if (settings.debug.value)
              log("Uninitialized variable " + tree + " at: " + (tree.pos));
            ast.Wildcard()
          } else {
            genExpression(rhs, ctx)
          }

          val rightOfEqual = if (hasSingleAssignSemantics(sym)) {
            value
          } else {
            ast.Apply(ast.Variable("NewCell"), List(value))
          }

          rightOfEqual match {
            case _: ast.Wildcard => ast.UnitVal()
            case _ =>
              ast.And(ast.Eq(varForSymbol(sym), rightOfEqual), ast.UnitVal())
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
          val source = genQualifier(fun, ctx)

          if (l.isValueType && r.isValueType) {
            if (cast)
              genConversion(l, r, source)
            else
              ast.Bool(l == r)
          }
          else if (l.isValueType) {
            val blackhole = blackholeReturnedValue(source)
            val result = if (cast) {
              ast.Raise(genBuiltinApply("New",
                  varForSymbol(definitions.ClassCastExceptionClass),
                  ast.Tuple(ast.Atom("<init>"), ast.Dollar())))
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

        // 'super' call: Note: since constructors are supposed to
        // return an instance of what they construct, we have to take
        // special care. On JVM they are 'void', and Scala forbids (syntactically)
        // to call super constructors explicitly and/or use their 'returned' value.
        // therefore, we can ignore this fact, and generate code that leaves nothing
        // on the stack (contrary to what the type in the AST says).
        case Apply(fun @ Select(sup @ Super(_, mix), _), args) =>
          if (settings.debug.value)
            log("Call to super: " + tree)

          val superClass = varForSymbol(sup.symbol) // TODO not correct
          val arguments0 = args map { genExpression(_, ctx) }
          val arguments = arguments0 ++ List(ast.Dollar())
          val message = ast.Tuple(atomForSymbol(fun.symbol), arguments:_*)

          ast.ObjApply(superClass, message)

        // 'new' constructor call: Note: since constructors are
        // thought to return an instance of what they construct,
        // we have to 'simulate' it by DUPlicating the freshly created
        // instance (on JVM, <init> methods return VOID).
        case Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          if (settings.debug.value)
            assert(ctor.isClassConstructor,
                   "'new' call to non-constructor: " + ctor.name)

          val classVar = varForSymbol(tpt.symbol)
          val arguments0 = args map { genExpression(_, ctx) }
          val arguments = arguments0 ++ List(ast.Dollar())
          val message = ast.Tuple(atomForSymbol(fun.symbol), arguments:_*)

          ast.Apply(ast.Variable("New"), List(classVar, message))

        case Apply(fun @ _, List(expr)) if (definitions.isBox(fun.symbol)) =>
          if (settings.debug.value)
            log("BOX : " + fun.symbol.fullName)

          // I think boxing is never needed in Oz
          genExpression(expr, ctx)

        case Apply(fun @ _, List(expr)) if (definitions.isUnbox(fun.symbol)) =>
          if (settings.debug.value)
            log("UNBOX : " + fun.symbol.fullName)

          genExpression(expr, ctx)

        case app @ Apply(fun, args) =>
          val sym = fun.symbol

          if (sym.isLabel) {  // jump to a label
            abort("Cannot jump to a label in Oz")
          } else {  // normal method call
            if (settings.debug.value)
              log("Gen CALL_METHOD with sym: " + sym + " isStaticSymbol: " + sym.isStaticMember);

            if (sym == ctx.method.symbol)
              ctx.method.recursive = true

            val instance = genQualifier(fun, ctx)
            val arguments0 = args map { genExpression(_, ctx) }
            val arguments = arguments0 ++ List(ast.Dollar())
            val message = ast.Tuple(atomForSymbol(fun.symbol), arguments:_*)

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
            // TODO get the 'this' of an outer class
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
            // TODO Review this
            ast.At(varForSymbol(sym))
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
              ast.StringLiteral(value.stringValue)
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

        case Assign(lhs @ Select(_, _), rhs) =>
          // TODO Review this
          val assignment = ast.Assign(varForSymbol(lhs.symbol),
              genExpression(rhs, ctx))
          ast.And(assignment, ast.Atom("unit"))

        case Assign(lhs, rhs) =>
          val sym = lhs.symbol
          val assignment = if (hasSingleAssignSemantics(sym))
            ast.Eq(varForSymbol(sym), genExpression(rhs, ctx))
          else
            ast.Assign(varForSymbol(sym), genExpression(rhs, ctx))
          ast.And(assignment, ast.Atom("unit"))

        case ArrayValue(tpt @ TypeTree(), _elems) =>
          ast.Atom("TODO: array item value")

        case Match(selector, cases) =>
          if (settings.debug.value)
            log("Generating SWITCH statement.")

          val expr = genExpression(selector, ctx)

          var clauses: List[ast.CaseClause] = Nil
          var elseClause: ast.OptElse = ast.NoElse()

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
          ast.Atom("unit")

        case _ =>
          abort("Unexpected tree in genLoad: " + tree + "/" + tree.getClass +
              " at: " + tree.pos)
      }
    }

    private def genStatement(tree: Tree, ctx: Context): ast.Phrase = {
      blackholeReturnedValue(genExpression(tree, ctx))
    }

    private def blackholeReturnedValue(expr: ast.Phrase): ast.Phrase = {
      expr match {
        case ast.Atom(_) => ast.Skip()

        case ast.And(statement, _:ast.Atom) => statement

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

    /** Generate the qualifier of `tree' */
    private def genQualifier(tree: Tree, ctx: Context): ast.Phrase =
      tree match {
        case Select(qualifier, _) =>
          genExpression(qualifier, ctx)
        case _ =>
          abort("Unknown qualifier " + tree)
      }

    private def genModule(ctx: Context, sym: Symbol, pos: Position) =
      varForSymbol(sym) setPos pos

    def genConversion(from: TypeKind, to: TypeKind, value: ast.Phrase) = {
      def noteq = ast.Atom("\\=")
      def int0 = ast.IntLiteral(0)
      def int1 = ast.IntLiteral(1)
      def float0 = ast.FloatLiteral(0.0)
      def float1 = ast.FloatLiteral(1.0)

      (from, to) match {
        case _ if from eq to => value

        case (INT, FLOAT) => genBuiltinApply("IntToFloat", value)
        case (FLOAT, INT) => genBuiltinApply("FloatToInt", value)

        case (INT, BOOL) => ast.BinaryOpApply(noteq, value, int0)
        case (FLOAT, BOOL) => ast.BinaryOpApply(noteq, value, float0)

        case (BOOL, INT) => ast.IfThenElse(value, int1, int0)
        case (BOOL, FLOAT) => ast.IfThenElse(value, float1, float0)
      }
    }

    def genCast(from: Type, to: Type, value: ast.Phrase, cast: Boolean) = {
      genBuiltinApply(if (cast) "AsInstance" else "IsInstance",
          value, varForSymbol(to.typeSymbol))
    }

    def genBuiltinApply(funName: String, args: ast.Phrase*) =
      ast.Apply(ast.Variable(funName), args.toList)

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
      ast.ClassDef(varForSymbol(clazz.symbol), makeClassDescriptors(clazz),
          makeClassMethods(clazz))
    }

    def makeClassDescriptors(clazz: OzClass): List[ast.ClassDescriptor] = {
      val withAttrs = if (clazz.fields.isEmpty)
        Nil
      else {
        val attrs = ast.Attr(clazz.fields.map {
          field => varForSymbol(field.symbol)
        })

        List(attrs)
      }

      val sym = clazz.symbol
      val bases = for (mixin <- sym.superClass :: sym.mixinClasses)
        yield varForSymbol(mixin)
      val withFrom = ast.From(bases) :: withAttrs

      withFrom
    }

    def makeClassMethods(clazz: OzClass): List[ast.MethodDef] = {
      clazz.methods.filter { _.code ne null }
                   .map { method => makeMethodDef(method) }
    }

    def makeMethodDef(method: OzMethod): ast.MethodDef = {
      val name = atomForSymbol(method.symbol)
      val args0 = method.params.map {
        local => ast.MethodArg(None, varForSymbol(local.sym), None)
      }
      val args = args0 ++ List(ast.MethodArg(None, ast.Dollar(), None))
      val body = method.code.asInstanceOf[ast.Phrase]

      val locals = method.locals filter {
        local => !method.params.contains(local)
      }

      val actualBody = if (locals.isEmpty)
        body
      else {
        val localVars = locals map { local => varForSymbol(local.sym) }
        ast.LocalDef(ast.And(localVars:_*), body)
      }

      ast.MethodDef(name, args, actualBody)
    }

    /////////////////// Module assembly ///////////////////////

    def makeModules(): List[ast.Eq] = {
      for (clazz <- unitClasses.toList if (clazz.symbol.isModuleClass))
        yield makeModule(clazz)
    }

    def makeModule(clazz: OzClass) = {
      val module = clazz.symbol.companionModule
      val moduleVar = varForSymbol(module)
      val tempVar = ast.Variable("X")

      val waitNeeded = genBuiltinApply("WaitNeeded", tempVar)
      val newCall = genBuiltinApply("New",
          varForSymbol(clazz.symbol),
          ast.Tuple(ast.Atom("<init>"), ast.Dollar()))

      val threadContents = ast.And(waitNeeded,
          ast.Eq(tempVar, newCall))
      val thread = ast.Thread(threadContents)

      val localStats = ast.And(thread, tempVar)
      val local = ast.LocalDef(tempVar, localStats)

      ast.Eq(moduleVar, local)
    }

    /* Symbol encoding */

    def varForSymbol(sym: Symbol) = {
      val name = if (sym.name.isTypeName)
        "type:" + sym.fullName
      else if (sym.isModule)
        "module:" + sym.fullName
      else
        sym.name.toString

      val suffix = if (sym.hasModuleFlag && !sym.isMethod &&
          !sym.isImplClass && !sym.isJavaDefined)
        "$"
      else
        ""

      ast.QuotedVar(name + suffix)
    }

    def atomForSymbol(sym: Symbol) =
      ast.Atom(sym.name.toString)
  }
}
