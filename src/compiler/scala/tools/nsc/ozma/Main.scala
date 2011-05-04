package scala.tools.nsc
package ozma

import java.io.File
import File.pathSeparator

import scala.tools.nsc.interactive.{ RefinedBuildManager, SimpleBuildManager }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.util.{ BatchSourceFile, FakePos }
import Properties.{ versionString, copyrightString, residentPromptString }

/** The main class for ozmac, a compiler for the programming language Ozma.
 */
object Main extends AnyRef with EvalLoop {
  val versionMsg = "Ozma compiler " +
    versionString + " -- " +
    copyrightString

  val prompt = residentPromptString

  var reporter: ConsoleReporter = _

  private def ozmacError(msg: String) {
    reporter.error(FakePos("ozmac"),
        msg + "\n  ozmac -help  gives more information")
  }

  def resident(compiler: Global) {
    loop { line =>
      val args = line.split(' ').toList
      val command = new CompilerCommand(args, new Settings(ozmacError))
      compiler.reporter.reset()
      new compiler.Run() compile command.files
    }
  }

  def process(args: Array[String]) {
    val ss       = new Settings(ozmacError)
    reporter     = new ConsoleReporter(ss)
    val command  = new CompilerCommand(args.toList, ss)
    val settings = command.settings

    if (settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (settings.Yidedebug.value) {
      settings.Xprintpos.value = true
      settings.Yrangepos.value = true
      val compiler = new interactive.Global(settings, reporter)
      import compiler.{ reporter => _, _ }

      val sfs = command.files map getSourceFile
      val reloaded = new interactive.Response[Unit]
      askReload(sfs, reloaded)

      reloaded.get.right.toOption match {
        case Some(ex) => reporter.cancelled = true // Causes exit code to be non-0
        case None => reporter.reset() // Causes other compiler errors to be ignored
      }
      askShutdown
    }
    else if (settings.Ybuilderdebug.value != "none") {
      def fileSet(files : List[String]) = Set.empty ++ (files map AbstractFile.getFile)

      val buildManager = settings.Ybuilderdebug.value match {
        case "simple"   => new SimpleBuildManager(settings)
        case _          => new RefinedBuildManager(settings)
      }
      buildManager.addSourceFiles(fileSet(command.files))

      // enter resident mode
      loop { line =>
        val args = line.split(' ').toList
        val command = new CompilerCommand(args.toList, settings)
        buildManager.update(fileSet(command.files), Set.empty)
      }
    }
    else {
      val compiler =
        if (settings.Yrangepos.value)
          new interactive.Global(settings, reporter) with OzmaGlobal
        else
          new Global(settings, reporter) with OzmaGlobal

      try {
        if (reporter.hasErrors)
          return reporter.flush()

        if (command.shouldStopWithInfo) {
          reporter.info(null, command.getInfoMessage(compiler), true)
        }
        else {
          if (settings.resident.value)
            resident(compiler)
          else if (command.files.isEmpty) {
            reporter.info(null, command.usageMsg, true)
            reporter.info(null, compiler.pluginOptionsHelp, true)
          }
          else {
            val run = new compiler.Run()
            run compile command.files
            reporter.printSummary()
          }
        }
      }
      catch {
        case ex =>
          compiler.logThrowable(ex)
          ex match {
            case FatalError(msg)  => reporter.error(null, "fatal error: " + msg)
            case _                => throw ex
          }
      }
    }
  }

  def main(args: Array[String]) {
    process(args)
    sys.exit(if (reporter.hasErrors) 1 else 0)
  }
}
