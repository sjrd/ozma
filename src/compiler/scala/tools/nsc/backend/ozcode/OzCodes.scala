/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Sébatien Doeraene
 */

package scala.tools.nsc
package backend
package ozcode

import ozma._

import java.io.PrintWriter
import scala.collection.mutable
import scala.tools.nsc.symtab._

/** Glue together OzCode parts.
 *
 *  @author Sébastien Doeraene
 */
abstract class OzCodes extends AnyRef with Members with ASTs with Natives
    with TypeKinds {
  val global: OzmaGlobal
  import global._

  /** The OzCode representation of classes */
  val classes = new mutable.HashMap[global.Symbol, OzClass]

  /** Debugging flag */
  def shouldCheckOzCode = settings.check contains global.ozcode.phaseName
  def checkerDebug(msg: String) =
    if (shouldCheckOzCode && global.opt.debug)
      println(msg)

  /** Print all classes and basic blocks. Used for debugging. */
  def dump {
    classes.values foreach (_.dump)
  }

  /** In order to support method overloading on the Mozart platform, we give
   *  every method a hash code that is appended to its name on the back-end.
   *  <p>This method computes the hash of a method.
   *  <p>Two methods should have the same hash if and only if they would
   *  <i>match</i> (as defined by the Scala reference, definition 5.1.4)
   *  if they had the same name. This ensures that proper overriding applies
   *  when running on Mozart.
   *  <p>As currently implemented, we only guarantee that two matching methods
   *  will have the same hash. We do not guarantee that two non-matching
   *  methods will have different hashes, though we try to achieve this on a
   *  best-effort basis.
   */
  def paramsHash(sym: Symbol) = sym.tpe match {
    case MethodType(params, _) =>
      params.foldLeft(0) { _ + _.tpe.typeSymbol.fullName.## }

    case NullaryMethodType(_) => 0

    case _ => abort("Expected a method type for " + sym)
  }

  def paramsHash(paramTypeNames: List[String]) =
    paramTypeNames.foldLeft(0) { _ + _.## }
}
