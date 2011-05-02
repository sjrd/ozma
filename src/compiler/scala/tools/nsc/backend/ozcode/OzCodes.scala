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
abstract class OzCodes extends AnyRef with Members with ASTs with TypeKinds {
  val global: OzmaGlobal
  import global.{ definitions, settings }

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
}
