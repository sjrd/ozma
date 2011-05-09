/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.nsc
package backend
package ozcode

trait TypeKinds { self: OzCodes =>
  import global._
  import definitions.{ ArrayClass, AnyRefClass, ObjectClass, NullClass, NothingClass, arrayType }

  lazy val ObjectReference = REFERENCE(definitions.ObjectClass)

  /** A map from scala primitive Types to OzCode TypeKinds */
  lazy val primitiveTypeMap: Map[Symbol, TypeKind] = {
    import definitions._
    Map(
      UnitClass     -> UNIT,
      BooleanClass  -> BOOL,
      CharClass     -> INT,
      ByteClass     -> INT,
      ShortClass    -> INT,
      IntClass      -> INT,
      LongClass     -> INT,
      FloatClass    -> FLOAT,
      DoubleClass   -> FLOAT
    )
  }

  /** Reverse map for toType */
  private lazy val reversePrimitiveMap: Map[TypeKind, Symbol] =
    primitiveTypeMap map (_.swap) toMap

  /**
   * This class represents a type kind. Type kinds represent the types that the
   * Mozart VM knows (or the OzCode view of what VM knows).
   */
  sealed abstract class TypeKind {
    def isReferenceType = false
    def isArrayType = false
    def isValueType = false

    def dimensions: Int = 0

    override def toString = {
      this.getClass.getName stripSuffix "$" dropWhile (_ != '$') drop 1
    }

    def toType: Type = reversePrimitiveMap get this map (_.tpe) getOrElse {
      this match {
        case REFERENCE(cls) => cls.tpe
        case ARRAY(elem) => arrayType(elem.toType)
        case _ => abort("Unknown type kind.")
      }
    }
  }

  sealed abstract class ValueTypeKind extends TypeKind {
    override def isValueType = true
  }

  /** Int */
  case object INT extends ValueTypeKind {}

  /** Float */
  case object FLOAT extends ValueTypeKind {}

  /** Boolean */
  case object BOOL extends ValueTypeKind {}

  /** The unit value */
  case object UNIT extends ValueTypeKind {}

  /** An object */
  case class REFERENCE(cls: Symbol) extends TypeKind {
    override def isReferenceType = false
  }

  /** An array */
  case class ARRAY(elem: TypeKind) extends TypeKind {
    override def toString = "ARRAY[" + elem + "]"
    override def isArrayType = true
    override def dimensions = elem.dimensions + 1

    /** The ultimate element type of this array. */
    def elementKind: TypeKind = elem match {
      case a @ ARRAY(_) => a.elementKind
      case k => k
    }
  }

  ////////////////// Conversions //////////////////////////////

  // The following code is a hard copy-and-paste from backend.icode.TypeKinds

  /** Return the TypeKind of the given type
   *
   *  Call to .normalize fixes #3003 (follow type aliases). Otherwise,
   *  arrayOrClassType below would return ObjectReference.
   */
  def toTypeKind(t: Type): TypeKind = t.normalize match {
    case ThisType(ArrayClass)            => ObjectReference
    case ThisType(sym)                   => REFERENCE(sym)
    case SingleType(_, sym)              => primitiveOrRefType(sym)
    case ConstantType(_)                 => toTypeKind(t.underlying)
    case TypeRef(_, sym, args)           => primitiveOrClassType(sym, args)
    case ClassInfoType(_, _, ArrayClass) => abort("ClassInfoType to ArrayClass!")
    case ClassInfoType(_, _, sym)        => primitiveOrRefType(sym)
    case ExistentialType(_, t)           => toTypeKind(t)
    case AnnotatedType(_, t, _)          => toTypeKind(t)
    // bq: useful hack when wildcard types come here
    // case WildcardType                    => REFERENCE(ObjectClass)
    case norm => abort(
      "Unknown type: %s, %s [%s, %s] TypeRef? %s".format(
        t, norm, t.getClass, norm.getClass, t.isInstanceOf[TypeRef]
      )
    )
  }

  /** Return the type kind of a class, possibly an array type.
   */
  private def arrayOrClassType(sym: Symbol, targs: List[Type]) = sym match {
    case ArrayClass       => ARRAY(toTypeKind(targs.head))
    case _ if sym.isClass => newReference(sym)
    case _                =>
      assert(sym.isType, sym) // it must be compiling Array[a]
      ObjectReference
  }

  /** Interfaces have to be handled delicately to avoid introducing
   *  spurious errors, but if we treat them all as AnyRef we lose too
   *  much information.
   */
  private def newReference(sym: Symbol): TypeKind = {
    // Can't call .toInterface (at this phase) or we trip an assertion.
    // See PackratParser#grow for a method which fails with an apparent mismatch
    // between "object PackratParsers$class" and "trait PackratParsers"
    if (sym.isImplClass) {
      // pos/spec-List.scala is the sole failure if we don't check for NoSymbol
      val traitSym = sym.owner.info.decl(nme.interfaceName(sym.name))
      if (traitSym != NoSymbol)
        return REFERENCE(traitSym)
    }
    REFERENCE(sym)
  }

  private def primitiveOrRefType(sym: Symbol) =
    primitiveTypeMap.getOrElse(sym, newReference(sym))
  private def primitiveOrClassType(sym: Symbol, targs: List[Type]) =
    primitiveTypeMap.getOrElse(sym, arrayOrClassType(sym, targs))
}
