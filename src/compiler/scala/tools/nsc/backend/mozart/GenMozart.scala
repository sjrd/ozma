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

      val OzCodeClasses(classes, modules) = unit.body
      val functors = makeFunctors(classes, modules)

      for (functor <- functors) {
        functor walk (_ setDefaultPos dummyPos)
        writeFunctor(functor)
      }

      this.unit = null
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
        "NewObject", "NewArrayObject", "AsInstance", "IsInstance",
        "ArrayClassOf", "MultiArrayClassOf", "StringLiteral",
        "AnyEqEq", "AnyRefEqEq",
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
      writeFunctorCode(functor)
      writeFunctorAST(functor)
    }

    def writeFunctorCode(functor: Functor) {
      val outfile = getFileFor(functor, ".oz")
      writeSyntax(functor, outfile)
    }

    def writeFunctorAST(functor: Functor) {
      val outfile = getFileFor(functor, ".ast.oz")
      val ast = functor.makeAST()
      writeSyntax(ast, outfile)
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
