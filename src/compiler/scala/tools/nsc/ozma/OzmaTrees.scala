package scala.tools.nsc
package ozma

import ast.Trees

trait OzmaTrees extends Trees { self: OzmaGlobal =>
  case class OzCodeClasses(classes: List[ozcodes.ast.ClassDef],
      modules: List[ozcodes.ast.Eq]) extends Tree
}
