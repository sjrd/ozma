package scala.tools.nsc
package ozma

import util.returning

import transform.{ ExplicitOuter, SpecializeTypes }

import backend.MozartPlatform
import backend.mozart._
import backend.ozcode._

trait OzmaGlobal extends Global with OzmaTrees {
  /** Platform */
  override lazy val platform: ThisPlatform =
    new { val global: OzmaGlobal.this.type = OzmaGlobal.this } with MozartPlatform

  /** OzCode generator */
  object ozcodes extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
  } with OzCodes

  // OZMA Compiler phases ------------------------------------------------------

  // phaseName = "singleass"
  object singleAssignVals extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
    val runsAfter = List[String]("parser")
    val runsRightAfter = None
    override val runsBefore = List[String]("namer")
  } with SingleAssignVals

  // phaseName = "looprecover"
  object whileLoopRecovering extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
    val runsAfter = List[String]("parser")
    val runsRightAfter = None
    override val runsBefore = List[String]("namer")
  } with WhileLoopRecovering

  // phaseName = "explicitouter"
  object ozmaExplicitOuter extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
    val runsAfter = List[String]("uncurry") // "tailcalls" in Global
    val runsRightAfter = None
  } with ExplicitOuter

  /*// phaseName = "specialize"
  object ozmaSpecializeTypes extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
    val runsAfter = List[String]("")
    val runsRightAfter = Some("uncurry") // "tailcalls" in Global
  } with SpecializeTypes*/

  // phaseName = "lambdalift"
  object ozmaLambdaLift extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
    val runsAfter = List[String]("lazyvals")
    val runsRightAfter = None
  } with OzmaLambdaLift

  // phaseName = "ozcode"
  object ozcode extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
    val runsAfter = List[String]("cleanup")
    val runsRightAfter = None
  } with GenOzCode

  // phaseName = "terminal"
  object ozmaTerminal extends {
    val global: OzmaGlobal.this.type = OzmaGlobal.this
    val phaseName = "terminal"
    val runsAfter = List[String]("mozart")
    val runsRightAfter = None
  } with SubComponent {
    private var cache: Option[GlobalPhase] = None
    def reset(): Unit = cache = None

    def newPhase(prev: Phase): GlobalPhase =
      cache getOrElse returning(new TerminalPhase(prev))(x => cache = Some(x))

    class TerminalPhase(prev: Phase) extends GlobalPhase(prev) {
      def name = "terminal"
      def apply(unit: CompilationUnit) {}
    }
  }

  /** Add the internal compiler phases to the phases set.
   *  This implementation creates a description map at the same time.
   */
  override protected def computeInternalPhases() {
    // Note: this fits -Xshow-phases into 80 column width, which it is
    // desirable to preserve.
    val phs = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      singleAssignVals        -> "take care of single assignment values",
      whileLoopRecovering     -> "recover while loops",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      superAccessors          -> "add super accessors in traits and nested classes",
      pickler                 -> "serialize symbol tables",
      refchecks               -> "reference/override checking, translate nested objects",
      uncurry                 -> "uncurry, translate function values to anonymous classes",
      //ozmaSpecializeTypes     -> "@specialized-driven class and method specialization",
      ozmaExplicitOuter       -> "this refs to outer pointers, translate patterns",
      erasure                 -> "erase types, add interfaces for traits",
      lazyVals                -> "allocate bitmaps, translate lazy vals into lazified defs",
      ozmaLambdaLift          -> "move nested functions to top level",
      constructors            -> "move field definitions into constructors",
      mixer                   -> "mixin composition",
      cleanup                 -> "platform-specific cleanups, generate reflective calls",
      ozcode                  -> "generate Oz code from AST",
      ozmaTerminal            -> "The last phase in the compiler chain"
    )

    phs foreach (addToPhasesSet _).tupled
  }
}
