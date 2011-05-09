package scala.tools.nsc
package ozma

import scala.collection.{ mutable, immutable }

import transform.Transform

abstract class WhileLoopRecovering extends Transform with ast.TreeDSL {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import CODE._

  val phaseName: String = "looprecover"

  def newTransformer(unit: CompilationUnit): Transformer =
    new WhileLoopRecoverer(unit)

  /**
   * Recover while loops that the parser has destroyed into labels and jumps
   */
  class WhileLoopRecoverer(unit: CompilationUnit) extends Transformer {
    import symtab.Flags._
    import lazyVals._

    def inScalaOzma(name: Name, pos: Position) = {
      Select(Select(Ident("scala") setPos pos, "ozma") setPos pos,
          name) setPos pos
    }

    /** Rewrite while-like label defs/calls as calls to
     *  `scala.ozma.whileLoop'
     */
    override def transform(tree: Tree): Tree = {
      tree match {
        // while (cond) { body }
        case LabelDef(lname, Nil,
            If(cond,
                Block(List(body), Apply(Ident(lname2), Nil)),
                Literal(_))) if (lname == lname2) =>
          val whileLoop = inScalaOzma("whileLoop", tree.pos)
          val innerApply = treeCopy.Apply(tree, whileLoop, List(cond))
          val outerApply = treeCopy.Apply(tree, innerApply, List(body))
          outerApply

        // do {body} while (cond)
        case LabelDef(lname, Nil,
            Block(List(body),
                If(cond,
                    Apply(Ident(lname2), Nil),
                    Literal(_)))) if (lname == lname2) =>
          val doWhileLoop = inScalaOzma("doWhileLoop", tree.pos)
          val innerApply = treeCopy.Apply(tree, doWhileLoop, List(body))
          val outerApply = treeCopy.Apply(tree, innerApply, List(cond))
          outerApply

        case _ => super.transform(tree)
      }
    }
  }
}
