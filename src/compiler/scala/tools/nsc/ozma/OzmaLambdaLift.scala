package scala.tools.nsc
package ozma

import transform.LambdaLift
import symtab.Flags._

abstract class OzmaLambdaLift extends LambdaLift {
  import global._

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase =
    new OzmaPhase(prev)

  /** The phase defined by this transform */
  class OzmaPhase(prev: scala.tools.nsc.Phase) extends Phase(prev) {
    override def apply(unit: global.CompilationUnit) {
      convertSingleAssignVars(unit, before = true)
      super.apply(unit)
      convertSingleAssignVars(unit, before = false)
      atPhase(phase.next)(propagateSingleAssign(unit))
    }

    def convertSingleAssignVars(unit: CompilationUnit, before: Boolean) {
      val singleAssignment = definitions.getClass("scala.ozma.singleAssignment")
      for (tree @ ValDef(_, _, _, _) <- unit.body;
          if (tree.symbol.hasAnnotation(singleAssignment))) {
        if (before)
          tree.symbol.resetFlag(MUTABLE)
        else
          tree.symbol.setFlag(MUTABLE)
      }
    }

    def propagateSingleAssign(unit: CompilationUnit) {
      val singleAssignment = definitions.getClass("scala.ozma.singleAssignment")

      for (tree @ Apply(s @ Select(n @ New(_), _), params) <- unit.body) {
        val clazz = n.tpe.typeSymbol
        if (clazz.isSynthetic) {
          for (param @ Ident(_) <- params;
              if (param.symbol.hasAnnotation(singleAssignment))) {
            val Some(annotInfo) = param.symbol.getAnnotation(singleAssignment)
            for (child <- clazz.info.decls) {
              if (child.isValue && (child.name == param.name) &&
                  child.hasAllFlags(SYNTHETIC | PARAMACCESSOR)) {
                child.addAnnotation(annotInfo)
              }
            }
          }
        }
      }
    }
  }
}
