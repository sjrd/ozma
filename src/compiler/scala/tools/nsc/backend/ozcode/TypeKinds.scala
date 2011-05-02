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
    def isValueType = false

    override def toString = {
      this.getClass.getName stripSuffix "$" dropWhile (_ != '$') drop 1
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
  case object OBJECT extends TypeKind {
    override def isReferenceType = false
  }

  ////////////////// Conversions //////////////////////////////

  /** Return the TypeKind of the given type
   *
   *  Call to .normalize fixes #3003 (follow type aliases). Otherwise,
   *  arrayOrClassType below would return ObjectReference.
   */
  def toTypeKind(t: Type): TypeKind = t.normalize match {
    case ThisType(sym)                   => OBJECT
    case SingleType(_, sym)              => symbolToTypeKind(sym)
    case ConstantType(_)                 => toTypeKind(t.underlying)
    case TypeRef(_, sym, args)           => symbolToTypeKind(sym)
    case ClassInfoType(_, _, sym)        => symbolToTypeKind(sym)
    case ExistentialType(_, t)           => toTypeKind(t)
    case AnnotatedType(_, t, _)          => toTypeKind(t)
    // PP to ID: I added RefinedType here, is this OK or should they never be
    // allowed to reach here?
    case RefinedType(parents, _)         => OBJECT
    // bq: useful hack when wildcard types come here
    // case WildcardType                    => OBJECT
    case norm => abort(
      "Unknown type: %s, %s [%s, %s] TypeRef? %s".format(
        t, norm, t.getClass, norm.getClass, t.isInstanceOf[TypeRef]
      )
    )
  }

  private def symbolToTypeKind(sym: Symbol) =
    primitiveTypeMap.getOrElse(sym, OBJECT)
}
