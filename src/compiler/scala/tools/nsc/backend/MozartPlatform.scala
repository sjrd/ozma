/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.nsc
package backend

import mozart.GenMozart

import ozma.OzmaGlobal

trait MozartPlatform extends JavaPlatform {
  val global: OzmaGlobal

  import global._

  object genMozart extends {
    val global: MozartPlatform.this.global.type = MozartPlatform.this.global
    val runsAfter = List[String]("ozcode", "tailcalls")
    val runsRightAfter = None
  } with GenMozart

  override def platformPhases = List(genMozart)
}
