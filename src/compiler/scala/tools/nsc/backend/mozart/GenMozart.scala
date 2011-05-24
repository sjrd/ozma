/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.nsc
package backend.mozart

import scala.collection.mutable.{ HashMap, ListBuffer, Set, HashSet }

import ozma._

import io.AbstractFile
import java.io.{FileOutputStream, OutputStreamWriter}

/** This is the compiler component that produces Mozart modules from Oz code
 *
 *  @author  Sébastien Doeraene
 *  @version 1.0
 *
 */
abstract class GenMozart extends OzmaSubComponent {
  import global._
  import definitions.ObjectClass
  import ozcodes._
  import ozcodes.ast.{ ClassDef, _ }

  val phaseName = "mozart"

  override def newPhase(p: Phase) = new MozartPhase(p)

  class MozartPhase(prev: Phase) extends StdPhase(prev) {
    var unit: CompilationUnit = _

    override def apply(unit: CompilationUnit): Unit = {
      this.unit = unit
      val dummyPos = new util.OffsetPosition(unit.source, 0)
      informProgress("Generating Mozart module for " + unit)

      val OzCodeClasses(classes) = unit.body
      val functors = makeFunctors(makeClasses(classes), makeModules(classes))

      for (functor <- functors) {
        functor walk (_ setDefaultPos dummyPos)
        writeFunctor(functor)
      }

      this.unit = null
    }

    lazy val SingleAssignmentClass =
      definitions.getClass("scala.ozma.singleAssignment")

    /////////////////// Class assembly ///////////////////////

    def makeClasses(classes: List[OzClass]): List[ast.ClassDef] = {
      val result: ListBuffer[ast.ClassDef] = new ListBuffer

      for (clazz <- classes)
        result += makeClass(clazz)

      result.toList
    }

    def makeClass(clazz: OzClass) = {
      ast.ClassDef(varForSymbol(clazz.symbol) setPos clazz.symbol.pos,
          makeClassDescriptors(clazz), makeClassMethods(clazz))
    }

    def makeClassDescriptors(clazz: OzClass): List[ast.ClassDescriptor] = {
      val withAttrs = if (clazz.fields.isEmpty)
        Nil
      else {
        val attrs = ast.Attr(clazz.fields.map { field =>
          val name = varForSymbol(field.symbol) setPos field.symbol.pos

          if (field.symbol.hasAnnotation(SingleAssignmentClass))
            name
          else {
            val initValue = genZeroOf(field.symbol)
            ast.InitAttrFeat(name, initValue)
          }
        })

        List(attrs)
      }

      val sym = clazz.symbol
      val withFrom = if (sym.isInterface)
        withAttrs
      else {
        val superClass = if (sym.superClass == NoSymbol)
          ObjectClass
        else
          sym.superClass

        val superClassVar = varForSymbol(superClass) setPos sym.pos
        (ast.From(List(superClassVar))) :: withAttrs
      }

      withFrom
    }

    def makeClassMethods(clazz: OzClass): List[ast.MethodDef] = {
      clazz.methods.filter { _.code ne null }
                   .map { method => makeMethodDef(method) }
    }

    def makeMethodDef(method: OzMethod): ast.MethodDef = {
      val name = atomForSymbol(method.symbol) setPos method.symbol.pos
      val args0 = method.params.map {
        local => ast.MethodArg(None,
            varForSymbol(local.sym).asInstanceOf[
              ast.MethodArgName] setPos local.sym.pos, None)
      }
      val resultParam = method.resultParam match {
        case Some(name) => ast.Variable(name)
        case None => ast.Dollar()
      }
      val args = args0 ++ List(ast.MethodArg(None, resultParam, None))
      val body = method.code.asInstanceOf[ast.Phrase]

      val locals = method.locals filter {
        local => !method.params.contains(local)
      }

      val labels = method.labels

      val actualBody = if (locals.isEmpty && labels.isEmpty)
        body
      else {
        val localVars = locals map { local =>
          varForSymbol(local.sym) setPos local.sym.pos
        }
        val defs = localVars ::: labels
        ast.LocalDef(ast.And(defs:_*), body) setPos method.symbol.pos
      }

      ast.MethodDef(name, args, actualBody) setPos method.symbol.pos
    }

    /////////////////// Module assembly ///////////////////////

    def makeModules(classes: List[OzClass]): List[ast.Eq] = {
      val modules = for (clazz <- classes
          if (clazz.symbol.isModuleClass &&
              (clazz.symbol.companionModule != NoSymbol)))
        yield makeModule(clazz)

      val classConstants = for (clazz <- classes)
        yield makeClassConstant(clazz)

      modules ::: classConstants
    }

    def makeModule(clazz: OzClass) = {
      val sym = clazz.symbol
      val module = sym.companionModule

      val atom = if (sym.isImplClass)
        ast.Atom("<init>#1063877011")
      else
        atomForSymbol(clazz.symbol.primaryConstructor)

      val value = genNew(clazz.symbol, Nil, atom)

      ast.Eq(varForSymbol(module), makeByNeed(value))
    }

    def makeClassConstant(clazz: OzClass) = {
      import definitions._

      val sym = clazz.symbol
      val fullName = ast.StringLiteral(sym.fullName + suffixFor(sym))

      val value = if (isPrimitiveType(sym)) {
        val encodedArrayType = toTypeKind(sym.tpe).toType.typeSymbol match {
          case UnitClass     => "U"
          case BooleanClass  => "Z"
          case CharClass     => "C"
          case ByteClass     => "B"
          case ShortClass    => "S"
          case IntClass      => "I"
          case LongClass     => "J"
          case FloatClass    => "F"
          case DoubleClass   => "D"
          case x => abort("Unknown primitive type: " + x.fullName)
        }

        val zero = genZeroOf(sym)

        val message = ast.Tuple(ast.Atom("<init>"),
            fullName, ast.Atom(encodedArrayType), zero, ast.Wildcard())

        genBuiltinApply("NewObject",
            ast.QuotedVar("type:java.lang.Class$PrimitiveClass"),
            varForClass(ClassClass), message)
      } else {
        val superClass = if (sym.superClass == NoSymbol)
          varForClass(ObjectClass)
        else
          varForClass(sym.superClass)
        val mixins = ast.ListLiteral((sym.mixinClasses map varForClass):_*)

        val rawAncestors = if (sym.ancestors.isEmpty)
          List(ObjectClass)
        else
          sym.ancestors
        val ancestors = ast.ListLiteral((rawAncestors map varForClass):_*)

        val arguments = List(fullName, superClass, mixins, ancestors)
        genNew(definitions.ClassClass, arguments, ast.Atom("<init>"))
      }

      ast.Eq(varForClass(clazz.symbol), makeByNeed(value))
    }

    private def isPrimitiveType(sym: Symbol) = toTypeKind(sym.tpe) match {
      case _:REFERENCE => false
      case _:ARRAY => false
      case _ => true
    }

    def makeByNeed(value: ast.Phrase) = {
      val fun = ast.Fun(ast.Dollar(), Nil, value)
      genBuiltinApply("ByNeed", fun)
    }

    /////////////////// Functor assembly ///////////////////////

    def makeFunctors(classes: List[ClassDef],
        modules: List[Eq]): List[Functor] = {
      val groups = gatherClasses(classes, modules)

      val functors = for ((groupName, (groupClasses, groupModules)) <- groups)
        yield makeFunctor(groupName, groupClasses, groupModules)

      functors.toList
    }

    def gatherClasses(classes: List[ClassDef],
        modules: List[Eq]) = {
      val groups = new HashMap[String, (ListBuffer[ClassDef], ListBuffer[Eq])]

      def groupFor(groupName: String) =
        groups.getOrElseUpdate(groupName,
          (new ListBuffer[ClassDef], new ListBuffer[Eq]))

      for (clazz <- classes) {
        val groupName = groupNameFor(clazz)
        groupFor(groupName)._1 += clazz
      }

      for (module <- modules) {
        val groupName = groupNameFor(module)
        groupFor(groupName)._2 += module
      }

      groups mapValues {
        case Pair(classes, modules) => (classes.toList, modules.toList)
      }
    }

    def groupNameFor(clazz: ClassDef): String = {
      val QuotedVar(varName) = clazz.name
      groupNameFor(varName)
    }

    def groupNameFor(module: Eq): String = {
      val Eq(QuotedVar(varName), _) = module
      groupNameFor(varName)
    }

    def groupNameFor(varName: String) = {
      val initFullName = varName.substring(varName.indexOf(':')+1)

      val fullName = if (varName startsWith "static:")
        initFullName.substring(0, initFullName.lastIndexOf('.'))
      else
        initFullName

      val dollar = fullName.indexOf('$')

      val baseName = if (dollar < 0)
        fullName
      else
        fullName.substring(0, dollar)

      baseName.stripSuffix(".")
    }

    def makeFunctor(name: String, classes: List[ClassDef],
        modules: List[Eq]) = {
      val definitions = And((classes ::: modules):_*)
      val imports = makeImports(name, definitions)
      val exports = makeExports(name, classes, modules)
      val define = Define(definitions)
      val descriptors = List(imports, exports, define)
      ast.Functor(ast.Dollar(), descriptors) setFullName name
    }

    private val isOzmaRuntimeBuiltin = List(
        "NewObject", "NewArrayObject", "ArrayValue", "AsInstance", "IsInstance",
        "ArrayClassOf", "MultiArrayClassOf", "StringLiteral",
        "AnyEqEq", "AnyRefEqEq", "NewActiveObject",
        "BinNot", "BinAnd", "BinOr", "BinXor", "LSL", "LSR", "ASR") toSet

    private val isOzSystemModule = List("System").toSet

    def makeImports(functorName: String, definitions: Node) = {
      val systemImports = new HashSet[String]
      val importsByGroup = new HashMap[String, Set[String]]

      def groupFor(groupName: String) =
        importsByGroup.getOrElseUpdate(groupName, new HashSet[String])

      val builtinGroup = groupFor("scala.ozma.OzmaRuntime")

      def importClass(varName: String) {
        val groupName = groupNameFor(varName)
        if (groupName == functorName)
          return // do not import classes defined in this functor

        val group = groupFor(groupName)
        group += varName
      }

      definitions walk {
        case QuotedVar(varName) if (varName contains ':') =>
          importClass(varName)
        case Variable(varName) if (isOzmaRuntimeBuiltin(varName)) =>
          builtinGroup += varName
        case Variable(varName) if (isOzSystemModule(varName)) =>
          systemImports += varName
        case _ => ()
      }

      val systemImportDecls = for (varName <- systemImports.toList)
        yield ImportItem(Variable(varName), Nil, NoImportAt())

      val importDecls = for ((name, varNames) <- importsByGroup)
        yield makeImportDecl(functorName, name, varNames)

      ast.Import(systemImportDecls ::: importDecls.toList)
    }

    def makeImportDecl(enclFunctorName: String, functorName: String,
        importedVarNames: Set[String]) = {
      val importItems = for (varName <- importedVarNames.toList) yield {
        val quoted = varName contains ':'
        AliasedFeature(if (quoted) QuotedVar(varName) else Variable(varName),
            Atom(varName))
      }

      val depth = enclFunctorName.foldLeft(0) { (res, c) =>
        if (c == '.') res+1 else res
      }

      val fileName = "x-ozma://root/" + functorName.replace('.', '/') + ".ozf"
      val importAt = ImportAt(Atom(fileName))
      ImportItem(QuotedVar("functor:" + functorName), importItems, importAt)
    }

    def makeExports(name: String, classes: List[ClassDef],
        modules: List[Eq]) = {
      val exportedClasses = for (clazz <- classes) yield {
        val QuotedVar(varName) = clazz.name
        ExportItem(ExportItemColon(Atom(varName), QuotedVar(varName)))
      }

      val exportedModules = for (module <- modules) yield {
        val Eq(QuotedVar(varName), _) = module
        ExportItem(ExportItemColon(Atom(varName), QuotedVar(varName)))
      }

      Export(exportedClasses ::: exportedModules)
    }

    /////////////////// Write to files ///////////////////////

    def writeFunctor(functor: Functor) {
      if (settings.verbose.value)
        writeFunctorCode(functor)
      writeFunctorAST(functor)
    }

    def writeFunctorCode(functor: Functor) {
      val outfile = getFileFor(functor, ".oz")
      writeSyntax(functor, outfile)
    }

    def writeFunctorAST(functor: Functor) {
      val outfile = getFileFor(functor, ".ast.oz")
      val outstream = new OutputStreamWriter(outfile.bufferedOutput, "US-ASCII")
      val ast = functor.makeAST()
      ast.save(outstream)
      outstream.close()
      compileASTToOZF(outfile)
    }

    def writeSyntax(node: Node, outfile: AbstractFile) {
      val outstream = new OutputStreamWriter(outfile.bufferedOutput, "US-ASCII")

      outstream.write(node.syntax())
      outstream.write("\n")
      outstream.close()
    }

    def compileASTToOZF(astfile: AbstractFile) {
      import java.io._

      val compiler = System.getenv("OZMA_HOME") + "/bin/ozastc"
      val commandLine = Array(compiler, "-c", "--nowarnunused", astfile.name)
      val workingDir = new File(astfile.container.path)
      val process = Runtime.getRuntime.exec(commandLine, null, workingDir)

      val input = new BufferedReader(new InputStreamReader(
          process.getErrorStream()))

      var line = input.readLine()
      while (line ne null) {
        scala.Console.println(line)
        line = input.readLine()
      }

      if (process.waitFor() != 0)
        abort("Oz AST compiler returned with error for file " + astfile)
    }

    def getFileFor(functor: Functor, suffix: String) = {
      var dir: AbstractFile =
        settings.outputDirs.outputDirFor(unit.source.file)

      val pathParts = functor.fullName.split("[./]").toList
      for (part <- pathParts.init)
        dir = dir.subdirectoryNamed(part)

      dir.fileNamed(pathParts.last + suffix)
    }
  }
}
