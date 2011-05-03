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
      informProgress("Generating Mozart module for " + unit)
      val OzCodeClasses(classes) = unit.body
      val functors = makeFunctors(classes)
      for (functor <- functors)
        writeFunctor(functor)
      this.unit = null
    }

    /////////////////// Functor assembly ///////////////////////

    def makeFunctors(classes: List[ClassDef]): List[Functor] = {
      val groups = gatherClasses(classes)

      val functors = for ((groupName, groupClasses) <- groups)
        yield makeFunctor(groupName, groupClasses)

      functors.toList
    }

    def gatherClasses(classes: List[ClassDef]) = {
      val groups = new HashMap[String, ListBuffer[ClassDef]]

      for (clazz <- classes) {
        val groupName = groupNameFor(clazz)

        val group = groups.get(groupName) match {
          case Some(group) => group
          case None =>
            val group = new ListBuffer[ClassDef]
            groups += (groupName -> group)
            group
        }

        group += clazz
      }

      groups mapValues { _.toList }
    }

    def groupNameFor(clazz: ClassDef): String = {
      val QuotedVar(varName) = clazz.name
      groupNameFor(varName)
    }

    def groupNameFor(varName: String) = {
      val fullName = varName.stripPrefix("type:")
      val dollar = fullName.indexOf('$')

      if (dollar < 0)
        fullName
      else
        fullName.substring(0, dollar)
    }

    def makeFunctor(name: String, classes: List[ClassDef]) = {
      val imports = makeImports(name, classes)
      val exports = makeExports(name, classes)
      val define = Define(And(classes:_*))
      val descriptors = List(imports, exports, define)
      ast.Functor(ast.Atom(name), descriptors)
    }

    def makeImports(name: String, classes: List[ClassDef]) = {
      val importsByGroup = new HashMap[String, Set[String]]

      def importClass(varName: String) {
        val groupName = groupNameFor(varName)
        if (groupName == name)
          return // do not import classes defined in this functor

        val group = importsByGroup.get(groupName) match {
          case Some(group) => group
          case None =>
            val group = new HashSet[String]
            importsByGroup += (groupName -> group)
            group
        }

        group += varName
      }

      for (clazz <- classes) {
        clazz walk {
          case QuotedVar(varName) if (varName.startsWith("type:")) =>
            importClass(varName)
          case _ => ()
        }
      }

      val importDecls = for ((name, varNames) <- importsByGroup)
        yield makeImportDecl(name, varNames)

      ast.Import(importDecls.toList)
    }

    def makeImportDecl(functorName: String, classVarNames: Set[String]) = {
      val importItems = for (varName <- classVarNames.toList)
        yield AliasedFeature(QuotedVar(varName), Atom(varName))

      val fileName = "./" + functorName.replace('.', '/') + ".ozf"
      val importAt = ImportAt(Atom(fileName))
      ImportItem(Variable("`"+functorName+"`"), importItems, importAt)
    }

    def makeExports(name: String, classes: List[ClassDef]) = {
      val exportItems = for (clazz <- classes) yield {
        val QuotedVar(varName) = clazz.name
        ExportItem(ExportItemColon(Atom(varName), QuotedVar(varName)))
      }

      Export(exportItems)
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
    }

    def writeSyntax(node: Node, outfile: AbstractFile) {
      val outstream = new OutputStreamWriter(outfile.bufferedOutput, "US-ASCII")
      
      outstream.write(node.syntax())
      outstream.write("\n")
      outstream.close()
    }

    def getFileFor(functor: Functor, suffix: String) = functor.name match {
      case ast.Atom(label) =>
        val dir: AbstractFile =
          settings.outputDirs.outputDirFor(unit.source.file)
        dir.fileNamed(label + suffix)
    }
  }
}
