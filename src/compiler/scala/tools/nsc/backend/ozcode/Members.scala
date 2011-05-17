/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Sébastien Doeraene
 */


package scala.tools.nsc
package backend
package ozcode

import java.io.PrintWriter
import scala.collection.{ mutable, immutable }
import mutable.{ HashMap, ListBuffer }
import symtab.Flags.{ DEFERRED }

trait ReferenceEquality {
  override def hashCode = System.identityHashCode(this)
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
}

trait Members { self: OzCodes =>
  import global._

  /** Common interface for OzClass/OzField/OzMethod. */
  trait OzMember extends Ordered[OzMember] {
    def symbol: Symbol

    def compare(other: OzMember) =
      if (symbol eq other.symbol) 0
      else if (symbol isLess other.symbol) -1
      else 1
  }

  /** Represent a class in OzCode */
  class OzClass(val symbol: Symbol) extends OzMember {
    var fields: List[OzField] = Nil
    var methods: List[OzMethod] = Nil
    var cunit: CompilationUnit = _
    var bootstrapClass: Option[String] = None

    def addField(f: OzField): this.type = {
      fields = f :: fields;
      this
    }

    def addMethod(m: OzMethod): this.type = {
      methods = m :: methods;
      this
    }

    def setCompilationUnit(unit: CompilationUnit): this.type = {
      this.cunit = unit;
      this
    }

    override def toString() = symbol.fullName

    def lookupField(s: Symbol) = fields find (_.symbol == s)
    def lookupMethod(s: Symbol) = methods find (_.symbol == s)
    def lookupMethod(s: Name) = methods find (_.symbol.name == s)

    /* returns this methods static constructor if it has one. */
    def lookupStaticCtor: Option[OzMethod] =
      methods find (_.symbol.isStaticConstructor)

    def dump() {
      methods foreach (_.dump)
    }
  }

  /** Represent a field in OzCode */
  class OzField(val symbol: Symbol) extends OzMember { }

  /**
   * Represents a method in OzCode.
   */
  class OzMethod(val symbol: Symbol) extends OzMember {
    var code: ast.Phrase = null
    var native = false

    /** The list of exception handlers, ordered from innermost to outermost. */
    var sourceFile: String = _

    var recursive: Boolean = false
    var hasReturn: Boolean = false

    /** local variables and method parameters */
    var locals: List[Local] = Nil

    /** method parameters */
    var params: List[Local] = Nil

    var resultParam: Option[String] = None

    def hasCode = code != null
    def setCode(code: ast.Phrase): OzMethod = {
      this.code = code
      this
    }

    def hasExpressionBody = hasCode && resultParam.isEmpty

    def addLocal(l: Local): Local =
      locals find (_ == l) getOrElse {
        locals ::= l
        l
      }

    def addParam(p: Local): Unit =
      if (params contains p) ()
      else {
        params ::= p
        locals ::= p
      }

    def addLocals(ls: List[Local]) = ls foreach addLocal
    def addParams(as: List[Local]) = as foreach addParam

    def lookupLocal(n: Name): Option[Local] = locals find (_.sym.name == n)
    def lookupLocal(sym: Symbol): Option[Local] = locals find (_.sym == sym)

    /** Is this method deferred? */
    def isAbstractMethod =
      symbol.isDeferred || symbol.owner.isInterface || native

    def isStatic: Boolean = symbol.isStaticMember

    override def toString() = symbol.fullName

    def dump {
      Console.println(this)
      Console.println(code)
      Console.println()
    }
  }

  /** Represent local variables and parameters */
  class Local(val sym: Symbol, val arg: Boolean) {
    var index: Int = -1

    /** Starting PC for this local's visibility range. */
    var start: Int = _

    /** Ending PC for this local's visibility range. */
    var end: Int = _

    /** PC-based ranges for this local variable's visibility */
    var ranges: List[(Int, Int)] = Nil

    override def equals(other: Any): Boolean = other match {
      case x: Local => sym == x.sym
      case _        => false
    }

    override def hashCode = sym.hashCode
    override def toString(): String = sym.toString
  }
}
