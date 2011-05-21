package scala.tools.nsc
package ozma

import scala.collection.{ mutable, immutable }

import transform.Transform

/** This component recovers while and do..while loops that the parser destroys
 *  in labels and jumps. It replaces them by calls to `scala.ozma.whileLoop'
 *  and `scala.ozma.doWhileLoop', respectively.
 */
abstract class WhileLoopRecovering extends Transform {
  import global._

  val phaseName: String = "looprecover"

  def newTransformer(unit: CompilationUnit): Transformer =
    new WhileLoopRecoverer(unit)

  /**
   * Recover while loops that the parser has destroyed into labels and jumps
   */
  class WhileLoopRecoverer(unit: CompilationUnit) extends Transformer {
    import symtab.Flags._

    def scalaOzmaDot(name: Name) =
      Select(gen.rootScalaDot("ozma"), name)

    /** Rewrite while-like label defs/calls as calls to
     *  `scala.ozma.whileLoop' and do-while-like label defs/calls as calls to
     *  `scala.ozma.doWhileLoop'.
     */
    override def transform(tree: Tree): Tree = {
      tree match {
        // while (cond) { body }
        case LabelDef(lname, Nil,
            If(cond,
                Block(List(body), Apply(Ident(lname2), Nil)),
                Literal(_))) if (lname == lname2) =>
          val whileLoop = atPos(tree.pos)(scalaOzmaDot("whileLoop"))
          val innerApply = treeCopy.Apply(tree, whileLoop, List(cond))
          val outerApply = treeCopy.Apply(tree, innerApply, List(body))
          outerApply

        // do {body} while (cond)
        case LabelDef(lname, Nil,
            Block(List(body),
                If(cond,
                    Apply(Ident(lname2), Nil),
                    Literal(_)))) if (lname == lname2) =>
          val doWhileLoop = atPos(tree.pos)(scalaOzmaDot("doWhileLoop"))
          val innerApply = treeCopy.Apply(tree, doWhileLoop, List(body))
          val outerApply = treeCopy.Apply(tree, innerApply, List(cond))
          outerApply

        case _ => super.transform(tree)
      }
    }
  }
}
