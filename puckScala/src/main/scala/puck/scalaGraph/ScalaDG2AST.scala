package puck.scalaGraph

import java.io.File

import puck.util.FileHelper._
import puck.{PuckPluginSettings, PuckPlugin, LoadingListener}
import puck.graph.{NodeId, DependencyGraph}
import puck.graph.io.{DG2AST, DG2ASTBuilder}
import puck.graph.transformations.Transformation
import puck.util.PuckLogger

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc._
import scala.tools.nsc.plugins.Plugin

class PuckGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter){

  val puckPlugin = new PuckPlugin(this)
  override def loadRoughPluginsList(): List[Plugin] =
    puckPlugin :: super.loadRoughPluginsList()
}

class PuckCompiler extends Driver {


  override protected def newCompiler(): PuckGlobal = new PuckGlobal(settings, reporter)

  //inspired from super.process
  def apply(args: List[String]) : (DependencyGraph, Global) = {
    val ss   = new Settings(scalacError)
    ss processArgumentString "-usejavacp" // use java classpath
    reporter = new ConsoleReporter(ss)
    command  = new CompilerCommand(args, ss)
    settings = command.settings

    val compiler = newCompiler()
    try {
      if (reporter.hasErrors)
        reporter.flush()
      else if (command.shouldStopWithInfo)
        reporter.echo(command.getInfoMessage(compiler))
      else
        doCompile(compiler)
    } catch {
      case ex: Throwable =>
        compiler.reportThrowable(ex)
        throw ex

    }
    (compiler.puckPlugin.genGraphComponent.g, compiler)

  }


}

object ScalaDG2AST extends DG2ASTBuilder {
  override def apply
  ( srcDirectory: File,
    outDirectory: File,
    jarListFile: File,
    logger: PuckLogger,
    ll: LoadingListener): DG2AST = {

    val srcs = findAllFiles(srcDirectory, ".scala", outDirectory.getName)
    val args = ("-Ystop-after:"+PuckPluginSettings.phaseName) :: srcs

    val (g, global) = new PuckCompiler()(args)

    new ScalaDG2AST(g, Seq(), Map())
  }
}
class ScalaDG2AST
( val initialGraph : DependencyGraph,
  val initialRecord : Seq[Transformation],
  val nodesByName : Map[String, NodeId]
  ) extends DG2AST {

  def apply(res: DependencyGraph)(implicit logger: PuckLogger): Unit = ???
  def printCode(dir: File)(implicit logger: PuckLogger): Unit = ???
  def parseConstraints(decouple: File)(implicit logger: PuckLogger): DG2AST = ???
  def code(graph: DependencyGraph, id: NodeId): String = ???

}
