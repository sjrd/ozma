package scala.tools.nsc
package backend.ozcode.opt

import _root_.scala.collection.Seq

import ozma.OzmaSubComponent
import symtab._

/** Tail call optimization
 */
abstract class TailCalls extends OzmaSubComponent {
  import global.ozcodes._
  import global.ozcodes.ast._

  val phaseName = "tailcalls"

  /** Create a new phase */
  override def newPhase(p: Phase) = new TailCallsPhase(p)

  /** Tail call optimization phase
   */
  class TailCallsPhase(prev: Phase) extends OzCodePhase(prev) {
    def name = phaseName

    override def apply(clazz: OzClass) {
      clazz.methods filter (m => m.hasCode && !m.native) foreach apply
    }

    def apply(method: OzMethod) {
      if (hasTailCall(method.code, method.hasExpressionBody)) {
        if (method.hasExpressionBody) {
          method.setCode(Eq(Variable("Result"), method.code))
          method.resultParam = Some("Result")
        }

        method.setCode(processTailCalls(method.code))
      }
    }

    // Search for tail calls

    object NewApply {
      def apply(typeVar: Variable, classVar: Variable, message: Record) = {
        Apply(Variable("NewObject"), List(typeVar, classVar, message))
      }

      def unapply(apply: Apply) = apply match {
        case Apply(Variable("NewObject"), List(typeVar, classVar, message)) =>
          Some((typeVar, classVar, message))
        case _ => None
      }
    }

    def hasTailCall(phrase: Phrase,
        isExpression: Boolean): Boolean = phrase match {
      // Apply's

      case apply @ ObjApply(cls, Tuple(label, args @ _*)) =>
        hasTailCall(args, apply.tailCallIndices)

      case apply @ MethodApply(proc, Tuple(label, args @ _*)) =>
        hasTailCall(args, apply.tailCallIndices)

      case apply @ NewApply(_, _, Tuple(label, args @ _*)) =>
        hasTailCall(args, apply.tailCallIndices)

      case apply @ Apply(proc, args) =>
        hasTailCall(args, apply.tailCallIndices)

      // Composite phrases: transfer to last component phrase

      case And(stats @ _*) =>
        hasTailCall(stats.last, isExpression)

      case Eq(_, rhs) if (!isExpression) =>
        hasTailCall(rhs, true)

      case IfThenElse(_, thenStats, elseStats) =>
        (hasTailCall(thenStats, isExpression) ||
            hasTailCall(elseStats, isExpression))

      case Case(_, clauses, elseClause) =>
        ((clauses exists (clause => hasTailCall(clause.body, isExpression))) ||
            hasTailCall(elseClause, isExpression))

      // Otherwise, considered atomic, so no tail call

      case _ => false
    }

    def hasTailCall(optElse: OptElse,
        isExpression: Boolean): Boolean = optElse match {
      case phrase:Phrase => hasTailCall(phrase, isExpression)
      case _ => false
    }

    def hasTailCall(args: Seq[Phrase], tailCallIndices: List[Int]) =
      !findTailCall(args, tailCallIndices).isEmpty

    def findTailCall(args: Seq[Phrase], tailCallIndices: List[Int]) = {
      val indexedArgs = args.toIndexedSeq
      val remaining = tailCallIndices.dropWhile { index =>
        indexedArgs(index) match {
          case _:GenericApply => false
          case _ => true
        }
      }
      remaining.firstOption
    }

    // Process tail calls

    def processTailCalls(statement: Phrase) = {
      statement
    }
  }
}
