/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package ozcode

import util.{Position, NoPosition}


/**
 * The OzCode intermediate representation. It is exactly an AST of the Mozart
 * Oz system. It uses the erased types of Scala and references Symbols to refer
 * named entities in the source files.
 */
trait ASTs { self: OzCodes =>
  import global.{Symbol, NoSymbol, Type, Name, Constant}

  /** This class represents a node of the intermediate AST code.
   *  Each case subclass will represent a specific operation.
   */
  abstract class Node extends Product with Cloneable {

    /** The corresponding position in the source file */
    private var _pos: Position = NoPosition

    def pos: Position = _pos

    def setPos(p: Position): this.type = {
      _pos = p
      this
    }

    def syntax(indent: String = ""): String

    override def toString = syntax()

    /** Clone this instruction. */
    override def clone: Node =
      super.clone.asInstanceOf[Node]

    val astLabel = {
      val fullName = getClass.getName
      val dollar = fullName.lastIndexOf('$')
      'f' + fullName.substring(dollar+1)
    }

    val hasCoord = true

    def makeAST(): ast.Phrase = {
      val label = ast.Atom(astLabel)
      val arguments = makeASTArguments()

      val actualArguments = if (hasCoord)
        arguments ++ List(makeCoord())
      else
        arguments

      ast.Tuple(label, actualArguments:_*)
    }

    protected def makeASTArguments(): List[ast.Phrase] =
      getASTArguments map { elem => makeASTArgument(elem) }

    protected def getASTArguments(): List[Any] = productIterator.toList

    protected def makeASTArgument(element: Any): ast.Phrase = element match {
      case node:Node => node.makeAST()

      case str:String => ast.Atom(str)

      case x:Long => ast.IntLiteral(x)
      case x:Double => ast.FloatLiteral(x)

      case seq:Seq[_] =>
        ast.ListLiteral(seq.toList.map { x => makeASTArgument(x) }:_*)

      case ASTNode(node) => node

      case _ => ast.Atom("unknown")
    }

    def makeCoord() = {
      ast.Atom("unit")
    }

    def walk[U](handler: Node => U) {
      handler(this)

      def inner(element: Any) {
        element match {
          case node:Node => node.walk(handler)
          case seq:Seq[_] => seq foreach inner
          case _ => ()
        }
      }

      productIterator foreach inner
    }
  }

  private case class ASTNode(node: ast.Phrase)

  object ast {
    // See the Mozart document at Syntax Tree Format

    // Categories
    trait RecordArgument extends Node
    trait ExportItemArg extends Node
    trait Pattern extends Node
    trait OptElse extends Node
    trait OptCatch extends Node
    trait OptFinally extends Node
    trait Phrase extends Node with RecordArgument with Pattern with OptElse
                              with OptFinally
    trait AttrOrFeat extends Node
    trait MethodName extends Node
    trait MethodArgName extends Node
    trait EscapedFeature extends Node with AttrOrFeat
    trait Feature extends Node
    trait FeatureNoVar extends Feature with EscapedFeature
    trait EscapableVariable extends Phrase with EscapedFeature with MethodName
    trait FunctorDescriptor extends Node
    trait ClassDescriptor extends Node

    // Auto-boxing

    implicit def int2literal(value: Int) = IntLiteral(value)
    implicit def float2literal(value: Double) = FloatLiteral(value)
    implicit def symbol2atom(value: scala.Symbol) = Atom(value.name)

    implicit def pair2colon(pair: Pair[Feature, Phrase]) = pair match {
      case Pair(feature, phrase) => Colon(feature, phrase)
    }

    // Actual nodes

    case class StepPoint(phrase: Phrase, kind: Atom) extends Phrase {
      def syntax(indent: String) = phrase.syntax(indent)
    }

    case class And(phrases: Phrase*) extends Phrase {
      def syntax(indent: String) = {
        val head :: tail = phrases.toList
        tail.foldLeft(head.syntax(indent)) {
          _ + "\n" + indent + _.syntax(indent)
        }
      }

      override val hasCoord = false

      override def makeAST(): ast.Phrase = makeAST(phrases.toList)

      def makeAST(list: List[Phrase]): Phrase = (list: @unchecked) match {
        case phrase :: Nil => phrase.makeAST()
        case head :: tail =>
          Tuple(ast.Atom(astLabel), head.makeAST(), makeAST(tail))
      }
    }

    trait InfixPhrase extends Phrase {
      val left: Phrase
      val right: Phrase
      protected val opSyntax: String

      def syntax(indent: String) = {
        val untilOp = left.syntax(indent) + opSyntax
        val rightIndent = indent + " "*untilOp.length
        untilOp + right.syntax(rightIndent)
      }
    }

    case class Eq(left: Phrase, right: Phrase) extends InfixPhrase {
      protected val opSyntax = " = "
    }

    case class Assign(left: Phrase, right: Phrase) extends InfixPhrase {
      protected val opSyntax = " := "
    }

    case class OrElse(left: Phrase, right: Phrase) extends InfixPhrase {
      protected val opSyntax = " orelse "
    }

    case class AndThen(left: Phrase, right: Phrase) extends InfixPhrase {
      protected val opSyntax = " andthen "
    }

    case class UnaryOpApply(operator: Atom, arg: Phrase,
        asStatement: Boolean = false) extends Phrase {
      override val astLabel =
        if (asStatement) "fOpApplyStatement" else "fOpApply"

      override protected def getASTArguments(): List[Any] =
        List(operator, List(arg))

      def syntax(indent: String) = {
        val op = operator.syntax(indent)
        op + arg.syntax(indent + " "*op.length)
      }
    }

    case class BinaryOpApply(operator: Atom, left: Phrase, right: Phrase,
        asStatement: Boolean = false) extends InfixPhrase {
      override val astLabel =
        if (asStatement) "fOpApplyStatement" else "fOpApply"

      override protected def getASTArguments(): List[Any] =
        List(operator, List(left, right))

      protected val opSyntax = operator.atom
    }

    case class ObjApply(superClass: Phrase, message: Phrase) extends Phrase {
      def syntax(indent: String) =
        superClass.syntax(indent) + ", " + message.syntax(indent)
    }

    case class At(cell: Phrase) extends Phrase {
      def syntax(indent: String) = "@" + cell.syntax(indent+" ")
    }

    case class Atom(
        atom: String) extends Phrase with FeatureNoVar with MethodName {
      def syntax(indent: String) = "'" + atom + "'"

      def apply(features: RecordArgument*) =
        Record(this, features:_*) setPos pos
    }

    abstract class SpecialAtom(val tag: String) {
      def apply() = Atom(tag)
      def unapply(atom: Atom) = atom.atom == tag
    }

    object UnitVal extends SpecialAtom("unit")

    object NullVal extends SpecialAtom("null")

    object True extends SpecialAtom("true")

    object False extends SpecialAtom("false")

    object Bool {
      def apply(value: Boolean) = if (value) True() else False()

      def unapply(atom: Atom) = atom match {
        case True() => Some(true)
        case False() => Some(false)
        case _ => None
      }
    }

    case class Variable(name: String) extends EscapableVariable
        with Feature with MethodArgName with ExportItemArg {
      override val astLabel = "fVar"

      def syntax(indent: String) = name
    }

    object QuotedVar {
      val QUOTE = '`'

      def apply(name: String) = Variable(QUOTE + name + QUOTE)

      def unapply(variable: Variable) = {
        val name = variable.name
        if (name.charAt(0) == QUOTE && name.charAt(name.length-1) == QUOTE)
          Some(name.substring(1, name.length-1))
        else
          None
      }
    }

    case class Escape(variable: Variable) extends EscapableVariable {
      def syntax(indent: String) = "!" + variable.syntax(indent+" ")
    }

    case class Wildcard() extends Phrase with MethodArgName {
      def syntax(indent: String) = "_"
    }

    case class Self() extends Phrase {
      def syntax(indent: String) = "self"
    }

    case class Dollar() extends Phrase with MethodArgName {
      def syntax(indent: String) = "$"
    }

    case class IntLiteral(value: Long) extends Phrase with FeatureNoVar {
      override val astLabel = "fInt"

      def syntax(indent: String) = value.toString
    }

    case class FloatLiteral(value: Double) extends Phrase {
      override val astLabel = "fFloat"

      def syntax(indent: String) = value.toString
    }

    case class Record(val label: Atom,
        val features: RecordArgument*) extends Phrase {

      override val hasCoord = false

      override def setPos(p: Position): this.type = {
        label.setPos(p)
        super.setPos(p)
      }

      def syntax(indent: String) = features.toList match {
        case Nil => label.syntax()

        case firstFeature :: otherFeatures => {
          val prefix = label.syntax() + "("
          val subIndent = indent + " " * prefix.length

          val firstLine = prefix + firstFeature.syntax(subIndent)

          otherFeatures.foldLeft(firstLine) {
            _ + "\n" + subIndent + _.syntax(subIndent)
          } + ")"
        }
      }
    }

    // The 'colon' record argument
    case class Colon(feature: Feature, phrase: Phrase) extends RecordArgument {
      override val hasCoord = false

      def syntax(indent: String) = {
        val prefix = feature.syntax(indent) + ":"
        prefix + phrase.syntax(indent + " "*prefix.length)
      }
    }

    object Tuple {
      def apply(label: Atom, features: Phrase*) = Record(label, features:_*)
    }

    case class ListLiteral(val items: Phrase*) extends Phrase {
      def syntax(indent: String) = items.toList match {
        case Nil => "nil"

        case firstItem :: otherItems => {
          val prefix = "["
          val subIndent = indent + " " * prefix.length

          val firstLine = prefix + firstItem.syntax(subIndent)

          otherItems.foldLeft(firstLine) {
            _ + "\n" + subIndent + _.syntax(subIndent)
          } + "]"
        }
      }

      override def makeAST() = toListTuple.makeAST()

      def toListTuple: Phrase = toListTuple(items.toList)

      protected def toListTuple(list: List[Phrase]): Phrase = (list match {
        case head :: tail => Tuple(ast.Atom("|"), head, toListTuple(tail))
        case Nil => ast.Atom("nil")
      }) setPos pos
    }

    case class StringLiteral(val value: String) extends Phrase {
      private lazy val asList = {
        val items = value.toList map { c => IntLiteral(c.asInstanceOf[Long]) }
        ListLiteral(items:_*) setPos pos
      }

      def syntax(indent: String) =
        if (value.isEmpty)
          "nil"
        else
          "\"" + value + "\""

      override def makeAST() = asList.makeAST()
    }

    case class Apply(fun: Phrase, args: List[Phrase]) extends Phrase {
      def syntax(indent: String) = args match {
        case Nil => "{" + fun.syntax() + "}"

        case firstArg :: otherArgs => {
          val prefix = "{" + fun.syntax() + " "
          val subIndent = indent + " " * prefix.length

          val firstLine = prefix + firstArg.syntax(subIndent)

          otherArgs.foldLeft(firstLine) {
            _ + "\n" + subIndent + _.syntax(subIndent)
          } + "}"
        }
      }
    }

    // Procedures and functions
    trait ProcOrFun extends Phrase {
      protected val keyword: String
      val name: Phrase
      val args: List[Phrase]
      val body: Phrase
      val flags: List[Atom]

      def syntax(indent: String) = {
        val flagsSyntax = flags.foldLeft("") { _ + " " + _.syntax(indent) }
        val argsSyntax = args.foldLeft("") { _ + " " + _.syntax(indent) }

        val header0 = keyword + flagsSyntax + " {" + name.syntax(indent)
        val header = header0 + argsSyntax + "}"

        val bodyIndent = indent + "   "
        val bodySyntax = bodyIndent + body.syntax(bodyIndent)

        header + "\n" + bodySyntax + "\n" + indent + "end"
      }
    }

    case class Proc(name: Phrase, args: List[Phrase], body: Phrase,
        flags: List[Atom] = Nil) extends ProcOrFun {
      protected val keyword = "proc"
    }

    case class Fun(name: Phrase, args: List[Phrase], body: Phrase,
        flags: List[Atom] = Nil) extends ProcOrFun {
      protected val keyword = "fun"
    }

    case class Functor(name: Phrase,
        descriptors: List[FunctorDescriptor]) extends Phrase {
      var fullName: String = _

      def setFullName(value: String): this.type = {
        fullName = value
        this
      }

      def syntax(indent: String) = {
        val firstLine = "functor " + name.syntax(indent + "        ") + "\n\n"
        descriptors.foldLeft(firstLine) {
          _ + indent + _.syntax(indent) + "\n\n"
        } + indent + "end"
      }
    }

    case class Import(importItems: List[ImportItem]) extends FunctorDescriptor {
      def syntax(indent: String) = {
        val subIndent = indent + "   "
        importItems.foldLeft("import") {
          _ + "\n" + subIndent + _.syntax(subIndent)
        }
      }
    }

    case class ImportItem(functor: Variable,
        aliasedFeatures: List[AliasedFeature],
        importAt: ImportAt) extends Node {
      override val hasCoord = false

      def syntax(indent: String) = {
        val withoutAt = aliasedFeatures.toList match {
          case Nil => functor.syntax(indent)

          case firstFeature :: otherFeatures => {
            val prefix = functor.syntax(indent) + "("
            val subIndent = indent + " " * prefix.length

            val firstLine = prefix + firstFeature.syntax(subIndent)

            otherFeatures.foldLeft(firstLine) {
              _ + "\n" + subIndent + _.syntax(subIndent)
            } + ")"
          }
        }

        withoutAt + importAt.syntax(indent)
      }
    }

    case class AliasedFeature(variable: Variable,
        feature: FeatureNoVar) extends Node {
      override val astLabel = "#"

      override val hasCoord = false

      def syntax(indent: String) = {
        val prefix = feature.syntax(indent) + ":"
        val subIndent = indent + " "*prefix.length
        prefix + variable.syntax(subIndent)
      }
    }

    case class ImportAt(url: Atom) extends Node {
      override val hasCoord = false

      def syntax(indent: String) = {
        " at " + url.syntax(indent + "    ")
      }
    }

    case class Export(exportItems: List[ExportItem]) extends FunctorDescriptor {
      def syntax(indent: String) = {
        val subIndent = indent + "   "
        exportItems.foldLeft("export") {
          _ + "\n" + subIndent + _.syntax(subIndent)
        }
      }
    }

    case class ExportItem(item: ExportItemArg) extends Node {
      override val hasCoord = false

      def syntax(indent: String) = item.syntax(indent)
    }

    // The 'colon' record argument
    case class ExportItemColon(feature: FeatureNoVar,
        variable: Variable) extends ExportItemArg {
      override val astLabel = "fColon"

      override val hasCoord = false

      def syntax(indent: String) = {
        val prefix = feature.syntax(indent) + ":"
        prefix + variable.syntax(indent + " "*prefix.length)
      }
    }

    case class Define(defs: Phrase,
        statements: Phrase = Skip()) extends FunctorDescriptor {
      def syntax(indent: String) = {
        val subIndent = indent + "   "
        val defsSyntax = "define\n" + subIndent + defs.syntax(subIndent)

        defsSyntax + (statements match {
          case Skip() => ""
          case _ => "\n" + indent + "in\n" + subIndent + statements.syntax(subIndent)
        })
      }
    }

    case class ClassDef(name: Phrase, descriptors: List[ClassDescriptor],
        methods: List[MethodDef]) extends Phrase {
      override val astLabel = "fClass"

      def syntax(indent: String) = {
        val header = "class " + name.syntax(indent + "      ")
        val subIndent = indent + "   "

        val descSyntax = descriptors.foldLeft(header) {
          _ + "\n" + subIndent + _.syntax(subIndent) + "\n"
        }

        val methSyntax = methods.foldLeft(descSyntax) {
          _ + "\n" + subIndent + _.syntax(subIndent) + "\n"
        }

        methSyntax + indent + "end"
      }
    }

    case class From(bases: List[Phrase]) extends ClassDescriptor {
      def syntax(indent: String) = {
        bases.foldLeft("from") { _ + " " + _.syntax(indent) }
      }
    }

    case class Prop(props: List[Phrase]) extends ClassDescriptor {
      def syntax(indent: String) = {
        props.foldLeft("prop") { _ + " " + _.syntax(indent) }
      }
    }

    case class Attr(attrs: List[AttrOrFeat]) extends ClassDescriptor {
      def syntax(indent: String) = {
        val subIndent = indent + "   "
        attrs.foldLeft("attr") {
          _ + "\n" + subIndent + _.syntax(subIndent)
        }
      }
    }

    case class Feat(features: List[AttrOrFeat]) extends ClassDescriptor {
      def syntax(indent: String) = {
        val subIndent = indent + "   "
        features.foldLeft("feat") {
          _ + "\n" + subIndent + _.syntax(subIndent)
        }
      }
    }

    case class MethodDef(name: MethodName, args: List[MethodArg],
        body: Phrase) extends Node {
      override val astLabel = "fMeth"

      private lazy val methodHead = new MethodHead

      def syntax(indent: String) = {
        val prefix = "meth " + name.syntax(indent)

        val header = (args match {
          case Nil => prefix
          case firstArg :: otherArgs => {
            val subIndent = indent + " "*prefix.length + " "
            otherArgs.foldLeft(prefix + "(" + firstArg.syntax(subIndent)) {
              _ + " " + _.syntax(subIndent)
            } + ")"
          }
        }) + "\n"

        val bodyIndent = indent + "   "
        header + bodyIndent + body.syntax(bodyIndent) + "\n" + indent + "end"
      }

      override protected def getASTArguments() =
        List(methodHead, body)

      private case class MethodHead() extends Node {
        override val astLabel = "fRecord"
        override val hasCoord = false

        def syntax(indent: String) = ""

        override protected def getASTArguments() =
          List(name, args)
      }
    }

    case class MethodArg(feature: Option[Feature], name: MethodArgName,
        default: Option[Phrase]) extends Node {
      override val astLabel = feature match {
        case None => "fMethArg"
        case Some(_) => "fColonMethArg"
      }

      override val hasCoord = false

      def syntax(indent: String) = {
        val untilColon = feature match {
          case None => ""
          case Some(feat) => feat.syntax(indent) + ":"
        }

        val untilName = untilColon + name.syntax(indent)

        untilName + (default match {
          case None => ""
          case Some(value) => " <= " + value.syntax(indent)
        })
      }

      override protected def getASTArguments() = {
        val defaultArg = ASTNode(default match {
          case None => Atom("fNoDefault")
          case Some(value) => Tuple(Atom("fDefault"), value) setPos value.pos
        })

        feature match {
          case None => List(name, defaultArg)
          case Some(feat) => List(feat, name, defaultArg)
        }
      }
    }

    case class LocalDef(defs: Phrase, statements: Phrase) extends Phrase {
      override val astLabel = "fLocal"

      def syntax(indent: String) = {
        val subIndent = indent + "   "

        val defsSyntax = "local\n" + subIndent + defs.syntax(subIndent)
        val statsSyntax = "in\n" + subIndent + statements.syntax(subIndent)

        defsSyntax + "\n" + indent + statsSyntax + "\n" + indent + "end"
      }
    }

    case class IfThenElse(condition: Phrase, thenStats: Phrase,
        elseStats: OptElse = NoElse()) extends Phrase {
      override val astLabel = "fBoolCase"

      def syntax(indent: String) = {
        val subIndent = indent + "   "

        val condSyntax = "if " + condition.syntax(subIndent) + " then"
        val thenSyntax = "\n" + subIndent + thenStats.syntax(subIndent)
        val elseSyntax = elseStats match {
          case NoElse() => ""
          case stats:Phrase =>
            "\n" + indent + "else\n" + subIndent + stats.syntax(subIndent)
        }

        condSyntax + thenSyntax + elseSyntax + "\n" + indent + "end"
      }
    }

    case class Case(expr: Phrase, clauses: List[CaseClause],
        elseClause: OptElse) extends Phrase {
      def syntax(indent: String) = {
        val firstLine = "case " + expr.syntax(indent + "     ")
        val subIndent = indent + "   "

        val firstClause :: otherClauses = clauses
        val secondLine =
          firstLine + "\n" + indent + "of " + firstClause.syntax(subIndent)

        val allClauses = otherClauses.foldLeft(secondLine) {
          _ + "\n" + indent + "[] " + _.syntax(subIndent)
        }

        val elseSyntax = elseClause match {
          case NoElse() => ""
          case stats:Phrase =>
            "\n" + indent + "else\n" + subIndent + stats.syntax(subIndent)
        }

        allClauses + elseSyntax + "\n" + indent + "end"
      }
    }

    case class CaseClause(pattern: Pattern, body: Phrase) extends Node {
      def syntax(indent: String) = {
        pattern.syntax(indent) + " then\n" + indent + body.syntax(indent)
      }
    }

    case class NoElse() extends OptElse {
      def syntax(indent: String) = "<noelse>"
    }

    case class Thread(body: Phrase) extends Phrase {
      def syntax(indent: String) = {
        val subIndent = indent + "   "

        "thread\n" + subIndent + body.syntax(subIndent) + "\n" + indent + "end"
      }
    }

    case class Try(body: Phrase, catches: OptCatch,
        finalizer: OptFinally) extends Phrase {
      def syntax(indent: String) = {
        val subIndent = indent + "   "
        val bodySyntax = "try\n" + subIndent + body.syntax(subIndent)

        val catchSyntax = catches match {
          case Catch(clauses) =>
            val firstClause :: otherClauses = clauses
            val firstLine =
              "\n" + indent + "catch " + firstClause.syntax(subIndent)

            otherClauses.foldLeft(firstLine) {
              _ + "\n" + indent + "[] " + _.syntax(subIndent)
            }

          case NoCatch() => ""
        }

        val finallySyntax = finalizer match {
          case NoFinally() => ""
          case stats:Phrase =>
            "\n" + indent + "finally\n" + subIndent + stats.syntax(subIndent)
        }

        bodySyntax + catchSyntax + finallySyntax + "\n" + indent + "end"
      }
    }

    case class Catch(clauses: List[CaseClause]) extends OptCatch {
      def syntax(indent: String) = "<catch>"
    }

    case class NoCatch() extends OptCatch {
      override val hasCoord = false

      def syntax(indent: String) = "<nocatch>"
    }

    case class NoFinally() extends OptFinally {
      override val hasCoord = false

      def syntax(indent: String) = "<nofinally>"
    }

    case class Raise(body: Phrase) extends Phrase {
      def syntax(indent: String) = {
        val subIndent = indent + "   "

        "raise\n" + subIndent + body.syntax(subIndent) + "\n" + indent + "end"
      }
    }

    case class Skip() extends Phrase {
      def syntax(indent: String) = "skip"
    }
  }
}
