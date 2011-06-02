package scala.tools.nsc
package ozma

import transform.Transform

/** This component annotates automatically the parameters of the primary
 *  constructor of case classes with @tailcall.
 */
abstract class CaseClassTailCalls extends Transform {
  import global._

  val phaseName: String = "casetailcalls"

  def newTransformer(unit: CompilationUnit): Transformer =
    new CaseClassTailCallsTransformer(unit)

  /**
   * Annotate the parameters of the primary constructor of case classes with
   * @tailcall
   */
  class CaseClassTailCallsTransformer(unit: CompilationUnit)
      extends Transformer {
    import symtab.Flags._

    def scalaOzmaDot(name: Name) =
      Select(gen.rootScalaDot("ozma"), name)

    override def transform(tree: Tree): Tree = tree match {
      case ClassDef(mods, name, tparams, impl) if (mods.hasFlag(CASE)) =>
        val Template(parents, self, body) = impl
        treeCopy.ClassDef(tree, mods, name, transformTypeDefs(tparams),
            treeCopy.Template(impl, parents, self,
                transformCaseClassBody(body)))

      case _ => super.transform(tree)
    }

    def transformCaseClassBody(body: List[Tree]): List[Tree] = {
      if (body exists isDangerousDef)
        body
      else
        body mapConserve transformCaseClassMember
    }

    def isDangerousDef(tree: Tree) = tree match {
      case _:Import => false
      case _:ClassDef | _:DefDef | _:TypeDef => false
      case ValDef(_, _, _, rhs) => rhs != EmptyTree
      case _ => true
    }

    def transformCaseClassMember(tree: Tree): Tree = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs)
      if ((name.toString == "<init>") && isEmptyCtorBody(rhs)) =>
        val newParams = vparamss mapConserve (_ mapConserve addTailCallAnnot)
        treeCopy.DefDef(tree, mods, name, tparams, newParams, tpt, rhs)

      case _ => transform(tree)
    }

    def isEmptyCtorBody(rhs: Tree) = rhs match {
      case Block(List(
          Apply(Select(Super(This(_), _), _), params)),
          Literal(Constant(()))) =>
        params forall (_.isInstanceOf[Ident])

      case _ => false
    }

    def addTailCallAnnot(value: ValDef): ValDef = {
      val ValDef(mods, name, tpt, rhs) = value
      val newMods = mods withAnnotations List(
          atPos(value.pos)(genTailCallAnnot()))
      treeCopy.ValDef(value, newMods, name, tpt, rhs)
    }

    def genTailCallAnnot() = {
      val tailcall = scalaOzmaDot("tailcall".toTypeName)
      Apply(Select(New(tailcall), "<init>"), List())
    }
  }
}
