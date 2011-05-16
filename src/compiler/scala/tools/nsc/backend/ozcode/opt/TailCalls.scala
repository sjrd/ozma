package scala.tools.nsc
package backend.ozcode.opt

import ozma.OzmaSubComponent
import symtab._

/** Tail call optimization
 */
abstract class TailCalls extends OzmaSubComponent {
  import global._
  import ozcodes._

  val phaseName = "tailcalls"

  /** Create a new phase */
  override def newPhase(p: Phase) = new TailCallsPhase(p)

  /** Tail call optimization phase
   */
  class TailCallsPhase(prev: Phase) extends OzCodePhase(prev) {
    def name = phaseName

    override def apply(clazz: OzClass) {
    }
  }
}
