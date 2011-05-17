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
          method.resultParam = Some("Result")
          method.setCode(processTailCalls(method.code,
              Some(Variable("Result"))))
        } else
          method.setCode(processTailCalls(method.code, None))
      }
    }

    // Search for tail calls

    object NewApply {
      def apply(typeVar: Variable, classVar: Variable, message: Record) = {
        Apply(Variable("NewObject"), List(typeVar, classVar, message))
      }

      def unapply(apply: Apply) = apply match {
        case Apply(Variable("NewObject"),
            List(typeVar:Variable, classVar:Variable, message)) =>
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

    def processTailCalls(phrase: Phrase, result: Option[Phrase]): Phrase = {
      lazy val isExpression = !result.isEmpty

      phrase match {
        // Apply's

        case apply @ ObjApply(cls, Tuple(label, args @ _*)) =>
          makeTailCall(result, args, apply.tailCallIndices,
              args => ObjApply(cls, Tuple(label, args:_*)))

        case apply @ MethodApply(obj, Tuple(label, args @ _*)) =>
          makeTailCall(result, args, apply.tailCallIndices,
              args => MethodApply(obj, Tuple(label, args:_*)))

        case apply @ NewApply(typeVar, classVar, Tuple(label, args @ _*)) =>
          makeTailCall(result, args, apply.tailCallIndices,
              args => NewApply(typeVar, classVar, Tuple(label, args:_*)))

        case apply @ Apply(proc, args) =>
          makeTailCall(result, args, apply.tailCallIndices,
              args => Apply(proc, args.toList))

        // Composite phrases: transfer to last component phrase

        case And(stats @ _*) =>
          val last :: others = stats.reverse.toList
          val newLast = processTailCalls(last, result)
          val newStats = (newLast :: others).reverse
          And(newStats:_*)

        case Eq(lhs, rhs) if (!isExpression) =>
          if (hasTailCall(rhs, true))
            processTailCalls(rhs, Some(lhs))
          else
            phrase

        case IfThenElse(cond, thenStats, elseStats) =>
          IfThenElse(cond, processTailCalls(thenStats, result),
              processTailCalls(elseStats, result))

        case Case(expr, clauses, elseClause) =>
          val newClauses = for (CaseClause(pattern, body) <- clauses)
            yield CaseClause(pattern, processTailCalls(body, result))
          val newElse = processTailCalls(elseClause, result)
          Case(expr, newClauses, newElse)

        // Otherwise, considered atomic, so no tail call

        case _ =>
          eqIfNeeded(result, phrase)
      }
    }

    def processTailCalls(optElse: OptElse, result: Option[Phrase]): OptElse = {
      optElse match {
        case phrase:Phrase => processTailCalls(phrase, result)
        case NoElse() => optElse
      }
    }

    def makeTailCall(result: Option[Phrase], args: Seq[Phrase],
        tailCallIndices: List[Int],
        makeApply: Seq[Phrase] => Phrase): Phrase = {
      def tempVar = Variable("TailCallTemp")

      findTailCall(args, tailCallIndices) match {
        case None => eqIfNeeded(result, makeApply(args))

        case Some(index) =>
          var tailCall: Phrase = null

          val newArgs = for ((arg, idx) <- args.zipWithIndex) yield {
            if (idx == index) {
              tailCall = arg
              tempVar
            } else
              arg
          }

          assert(tailCall ne null)

          LocalDef(
              tempVar,
          And( // in
              eqIfNeeded(result, makeApply(newArgs)),
              Eq(tempVar, tailCall)
          ))
      }
    }

    private def eqIfNeeded(result: Option[Phrase], phrase: Phrase) = {
      if (result.isEmpty)
        phrase
      else
        Eq(result.get, phrase)
    }
  }
}
