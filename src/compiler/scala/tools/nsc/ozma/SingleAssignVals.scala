package scala.tools.nsc
package ozma

import scala.collection.{ mutable, immutable }

import transform.Transform

abstract class SingleAssignVals extends Transform with ast.TreeDSL {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import CODE._

  val phaseName: String = "singleass"

  def newTransformer(unit: CompilationUnit): Transformer =
    new SingleAssignValues(unit)

  /**
   * Take care of single-assignment local values
   */
  class SingleAssignValues(unit: CompilationUnit) extends Transformer {
    import symtab.Flags._
    import lazyVals._

    var isInBlock = false

    def setInBlock[A](value: Boolean)(stat: => A) = {
      val savedIsInBlock = isInBlock
      isInBlock = value
      val result = stat
      isInBlock = savedIsInBlock
      result
    }

    def inBlock[A](stat: => A) = setInBlock(true)(stat)

    def notInBlock[A](stat: => A) = setInBlock(false)(stat)

    def inScalaOzma(name: Name, pos: Position) = {
      Select(Select(Ident("scala") setPos pos, "ozma") setPos pos,
          name) setPos pos
    }

    def typeInScalaOzma(name: Name, pos: Position) =
      gen.convertToTypeName(inScalaOzma(name, pos)) get

    def singleAssignAnnot(pos: Position) =
      New(typeInScalaOzma("singleAssignment", pos), List(Nil)) setPos pos

    /** Perform the following transformations:
     *  - for any value in a Block of the form:
     *    `val value: Type`
     *    rewrite it as:
     *    `@scala.ozma.singleAssignment var value: Type = scala.ozma.newUnbound`
     */
    override def transform(tree: Tree): Tree = {
      tree match {
        case ValDef(mods, name, tpt, EmptyTree)
        if (isInBlock && !mods.hasFlag(MUTABLE | LAZY)) =>
          val annots = List(singleAssignAnnot(tree.pos))
          val newmods = (mods &~ DEFERRED | MUTABLE) withAnnotations annots
          val rhs = inScalaOzma("newUnbound", tree.pos)
          treeCopy.ValDef(tree, newmods, name, tpt, rhs)

        case Block(_, _) => inBlock(super.transform(tree))

        case _ => notInBlock(super.transform(tree))
      }
    }
  }
}
