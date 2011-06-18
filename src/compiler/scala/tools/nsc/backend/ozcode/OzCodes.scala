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
  import definitions.ArrayClass

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

  def buildMessage(label: ast.Atom, arguments: List[ast.Phrase],
      additionalArg: ast.Phrase = ast.Dollar()) =
    ast.Tuple(label, (arguments ++ List(additionalArg):_*))

  def genNew(clazz: Symbol, arguments: List[ast.Phrase] = Nil,
      label: ast.Atom = null) = {
    val actualLabel = if (label ne null)
      label
    else
      atomForSymbol("<init>", paramsHash(List(clazz.fullName)))

    val typeVar = varForSymbol(clazz)
    val classVar = varForClass(clazz)
    val message = buildMessage(actualLabel, arguments, ast.Wildcard())
    genBuiltinApply("NewObject", typeVar, classVar, message)
  }

  def genNewArray(elementKind: TypeKind, dimensions: Int,
      arguments: List[ast.Phrase]) = {
    val argsLength = arguments.length

    if (argsLength > dimensions)
      abort("too many arguments for array constructor: found " + argsLength +
        " but array has only " + dimensions + " dimension(s)")

    val componentClass = elementKind.toType

    genBuiltinApply("NewArrayObject", genClassConstant(componentClass),
        ast.IntLiteral(dimensions), ast.ListLiteral(arguments:_*))
  }

  def genBuiltinApply(funName: String, args: ast.Phrase*) = {
    val pos = if (args.isEmpty) NoPosition else args(0).pos
    ast.Apply(ast.Variable(funName) setPos pos, args.toList) setPos pos
  }

  def genZeroOf(sym: Symbol): ast.Constant = genZeroOf(sym.tpe)

  def genZeroOf(tpe: Type): ast.Constant = toTypeKind(tpe) match {
    case UNIT => ast.UnitVal()
    case BOOL => ast.False()
    case INT(_) => ast.IntLiteral(0)
    case FLOAT(_) => ast.FloatLiteral(0.0)
    case REFERENCE(_) => ast.NullVal()
    case ARRAY(_) => ast.NullVal()
  }

  lazy val tailcallAnnot = definitions.getClass("scala.ozma.tailcall")

  lazy val isCoreTailCallSymbol = makeCoreTailCallSymbols()

  /* We need this dirty hard-coded computation for some core case classes
   * because our compiler still relies on Java classpaths. Hence when
   * compiling an application, :: e.g. is the standard Scala :: class, which
   * does not have the @tailcall annotation.
   */
  def makeCoreTailCallSymbols() = {
    import definitions._
    import definitions.getClass

    val classes = List(
        ConsClass, SomeClass,
        definitions.getClass("scala.Left"),
        definitions.getClass("scala.Right")
    ) ++ (TupleClass toList)

    val constructors = classes map (_.primaryConstructor) toSet

    val applies = classes map { clazz =>
      getMember(clazz.companionModule, "apply")
    } toSet

    constructors ++ applies
  }

  /** Compute the tail-call info of a method for use by the tailcalls phase */
  def computeTailCallInfo(method: Symbol): List[Int] = {
    if (isCoreTailCallSymbol(method)) {
      val MethodType(params, _) = method.tpe
      (params.length-1 to 0 by -1) toList
    } else {
      method.tpe match {
        case MethodType(params, _) =>
          val indices = for ((param, idx) <- params.view.zipWithIndex
              if (param.hasAnnotation(tailcallAnnot)))
            yield idx
          indices.reverse.toList

        case NullaryMethodType(resultType) =>
          Nil

        case _ => abort("Expected a method type for " + method)
      }
    }
  }

  /* Symbol encoding */

  def varForSymbol(sym: Symbol): ast.Phrase with ast.EscapedFeature = {
    val name = if (sym.name.isTypeName)
      "type:" + sym.fullName
    else if (sym.isModule)
      throw new AssertionError("varForSymbol for module requested" + sym)
    else if (sym.isStaticMember)
      throw new AssertionError("varForSymbol for static member requested" + sym)
    else if (sym.isLabel)
      "label~" + sym.name + "~" + sym.id
    else if (sym.owner.isClass && !(sym.name.toString endsWith " "))
      " " + sym.name.toString
    else if (sym.owner.isMethod && (!sym.isParameter))
      sym.name.toString + "~" + sym.id
    else
      sym.name.toString

    if ((name contains ':') || sym.isPrivate || sym.isLocal)
      ast.QuotedVar(name + suffixFor(sym))
    else
      ast.Atom(name + suffixFor(sym))
  }

  def genClassConstant(tpe: Type): ast.Phrase = {
    toTypeKind(tpe) match {
      case array @ ARRAY(_) =>
        val elementClass = varForClass(array.elementKind.toType.typeSymbol)
        genBuiltinApply("MultiArrayClassOf", elementClass,
            ast.IntLiteral(array.dimensions))

      case _ => varForClass(tpe.typeSymbol)
    }
  }

  def varForClass(sym: Symbol) = {
    val name = "class:" + sym.fullName
    ast.QuotedVar(name + suffixFor(sym))
  }

  def varForModuleInternal(sym: Symbol) = {
    ast.QuotedVar("modulevar~" + sym.fullName + "$")
  }

  def varForModule(sym: Symbol) = {
    ast.QuotedVar("module:" + sym.fullName + "$")
  }

  def suffixFor(sym: Symbol) =
    if (sym.hasModuleFlag && !sym.isMethod && !sym.isImplClass) "$" else ""

  def atomForSymbol(sym: Symbol): ast.Atom = {
    val name = sym.name.toString
    val hash = if (sym.isMethod) paramsHash(sym) else 0
    atomForSymbol(name, hash)
  }

  def atomForSymbol(name: String, hash: Int) =
    ast.Atom(if (hash != 0) name + "#" + hash else name)

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
  def paramsHash(sym: Symbol): Int = {
    sym.tpe match {
      case MethodType(params, resultType) =>
        paramsHash(params.toList map (_.tpe), resultType)

      case NullaryMethodType(resultType) =>
        paramsHash(Nil, resultType)

      case _ => abort("Expected a method type for " + sym)
    }
  }

  private def typeFullName(tpe: Type): String = tpe match {
    case ArrayType(elementType) => typeFullName(elementType) + "[]"
    case _ => tpe.typeSymbol.fullName
  }

  def paramsHash(paramTypes: List[Type], resultType: Type): Int =
    paramsHash(paramTypes map typeFullName, typeFullName(resultType))

  def paramsHash(paramTypeNames: List[String], resultTypeName: String): Int =
    paramsHash(paramTypeNames ::: List(resultTypeName))

  def paramsHash(paramAndResultTypeNames: List[String]) = {
    paramAndResultTypeNames.view.zipWithIndex.foldLeft(0) {
      case (prev, (item, idx)) => prev ^ Integer.rotateLeft(item.##, idx)
    }
  }

  /** Extractor object to match array types
   */
  object ArrayType {
    def unapply(typeRef: TypeRef) = typeRef match {
      case TypeRef(_, ArrayClass, List(elementType)) => Some(elementType)
      case _ => None
    }
  }

  /** A phase which works on ozcode */
  abstract class OzCodePhase(prev: Phase) extends global.GlobalPhase(prev) {
    override def erasedTypes = true

    override def apply(unit: global.CompilationUnit): Unit = {
      val OzCodeClasses(classes) = unit.body
      classes foreach apply
    }

    def apply(clazz: global.ozcodes.OzClass): Unit
  }
}
