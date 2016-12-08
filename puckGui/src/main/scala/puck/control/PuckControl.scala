/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.control

import java.io.File

import puck._
import puck.config.Config.Config
import puck.config.{Config, ConfigParser}
import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io.{CSVPrinter, DotPrinter}
import puck.graph.transformations.MutabilitySet
import puck.view.search.MetricChoice
import puck.view.{NodeKindIcons, SearchDialog}
import puck.util.{PuckLog, PuckLogger}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.swing.{ProgressBar, Publisher, Reactor, Swing}
import scala.util.{Failure, Success}
import scalaz.{-\/, \/-}
object PuckControl {

  def apply(graphUtils: GraphUtils, nodeKindIcons : NodeKindIcons, logger : PuckLogger) : PuckControl =
    new PuckControl(graphUtils, nodeKindIcons, logger)

  def apply(graphUtils: GraphUtils, nodeKindIcons : NodeKindIcons, f : File, logger : PuckLogger)  : PuckControl = {
    val pc = this apply (graphUtils, nodeKindIcons, logger)
    val sConf =
      if(f.isDirectory && Config.defaultConfFile(f).exists())
        Some(Config.defaultConfFile(f))
      else if(f.isFile) Some(f)
      else None

    sConf foreach pc.loadConf

    pc
  }
}

class PuckControl
(val graphUtils: GraphUtils,
 val nodeKindIcons : NodeKindIcons,
 implicit val logger: PuckLogger)
  extends Reactor {

  var sProject : Option[Project] = None

  val progressBar = new ProgressBar {
    min = 0
    max = 100
    value = 0
    labelPainted = true
    visible = false
  }

  object Bus extends Publisher
  this listenTo Bus


  var dg2ast: DG2AST = _
  val historyHandler: HistoryHandler = new GraphStack(Bus)
  var constraints : Option[ConstraintsMaps] = None
  var mutabilitySet : MutabilitySet.T = Set()

  var currentMetric : DependencyGraph => Int =
    new (DependencyGraph => Int) {
      override def toString = "Number of nodes"
      def apply(g : DependencyGraph) = Metrics.fitness0(g)
    }


  def metricChoices : Seq[MetricChoice] = {
    val cm = constraints.get
    val m1 = MetricChoice.metric1(cm)
    val m2 = MetricChoice.metric2(graph, cm)

    Seq(m1, m2)
  }


  def chooseMetric = {
    val cs = metricChoices
    MetricChoice.dialog(cs:_*) match {
      case Some(f) => f
      case None => ()
        Bus publish Log(cs.head + " selected by default")
        cs.head.metric
    }
  }

  def search() : Unit = SearchDialog(this)

  def loadConf(file : File) : Unit =  loadConf(ConfigParser(file))
  def loadConf(cfg : Config) : Unit = {
    sProject = Some(new Project(cfg, graphUtils.dg2astBuilder))

    sProject.foreach {
      p =>
        DotPrinter.dotPath = p.graphvizDot.map(_.getAbsolutePath)
    }

    val sf : Option[File]= sProject flatMap (_.someFile(Config.Keys.workspace))
    val path = sf map (_.getAbsolutePath) getOrElse "No directory selected"
    logger writeln  s"Workspace directory :\n$path"
    loadCodeAndConstraints(sProject.get)


  }

  def graph: DependencyGraph = historyHandler.graph

  import PuckLog.defaultVerbosity


  private[this] var printingOptionsControl0: PrintingOptionsControl = _

  def printingOptionsControl = printingOptionsControl0


  def loadCodeAndConstraints(project : Project) = Future {
    progressBar.visible = true
    progressBar.value = 0
      if (project.pathList(Config.Keys.srcs).isEmpty)
        throw NoSourceDetected

      dg2ast = project.loadGraph(Some(new LoadingListener {
        override def update(loading: Double): Unit =
          progressBar.value = (loading * 100).toInt
      }))
      progressBar.visible = false


      printingOptionsControl0 = PrintingOptionsControl(dg2ast.initialGraph, Bus)

      mutabilitySet = dg2ast.initialMutability
  } onComplete {
    case Success(_) =>
      logger writeln s"Graph builded : ${dg2ast.initialGraph.nodes.size} nodes"
      loadConstraints(setInitialGraph = true)
    case Failure(exc) =>
      progressBar.visible = false

      if(exc.getCause == null ) {
        if(exc.getMessage == null)
          logger write (exc.getStackTrace mkString "\n")
        else {
          logger writeln exc.getMessage
          exc.printStackTrace()
        }
      }
      else {
          if (exc.getCause != NoSourceDetected)
            exc.printStackTrace()
        logger writeln exc.getCause.getMessage
      }

  }


  def loadConstraints(setInitialGraph : Boolean = false) : Unit =
    sProject foreach {
      project =>
        logger.writeln("Loading constraints ...")


        constraints = project.parseConstraints(dg2ast)
        logger write " done :"

        if(setInitialGraph)
          historyHandler.setInitialGraph(dg2ast.initialGraph)

        constraints match {
          case None =>
            if(setInitialGraph)
              Bus publish GraphUpdate(graph)
            logger writeln "no constraints found"
          case Some(cm) =>
            logger.writeln((graph,cm).shows)
            Bus publish ConstraintsUpdate(graph, cm)
        }


    }


  def printRecording() : Unit =
    graph.recording.reverseIterator.foreach(r => logger writeln (graph, r).shows)



  def saveRecordOnFile(file : File) : Unit = {
    Recording.write(file.getAbsolutePath, dg2ast.nodesByName, graph)
  }

  def loadRecord(file : File) : Unit = {
    try historyHandler.load(Recording.load(file.getAbsolutePath, dg2ast.nodesByName))
    catch {
      case e @ Recording.LoadError(msg, m) =>
        println(e.printStackTrace())
        logger writeln ("Record loading error " + msg)
        logger writeln ("cannot bind loaded map " + m.toList.sortBy(_._2).mkString("\n"))
        logger writeln ("with " + dg2ast.nodesByName.toList.sortBy(_._2).mkString("\n"))
    }

  }


  reactions += {

    case PushGraph(g) =>
      historyHandler.pushGraph(g)

    case PrintErrOrPushGraph(msg, lgt) =>
      lgt.value match {
        case -\/(err) =>
          logger.writeln(s"$msg\n$err\nLog : ${lgt.log}")
        case \/-(g) =>
          logger.writeln(lgt.log)
          historyHandler.pushGraph(g)
      }

    case RewriteHistory(r) =>
      historyHandler.rewriteHistory(r)

    case Log(msg) =>
      logger.writeln(msg)

    case gf @ GraphFocus(g, e) =>
      printingOptionsControl.focus(g, e)

    case LoadCodeRequest =>
      sProject match {
        case None =>
          logger writeln "select a project first"
        case Some(p) => loadCodeAndConstraints(p)
      }


    case LoadConstraintRequest => loadConstraints()

    case SaveRecord(f) =>
      saveRecordOnFile(f)

    case LoadRecord(f) =>
      loadRecord(f)


    case ConstraintDisplayRequest(graph) =>
      logger writeln (constraints match {
        case None => "no constraints"
        case Some(cm) => (graph,cm).shows
      })

    case pe : PrintingOptionEvent =>
      pe(printingOptionsControl)

    case GenCode(compareOutput) =>
      sProject foreach { p =>
        import ProjectDG2ASTControllerOps._
        deleteOutDirAndApplyOnCode(dg2ast,p, historyHandler.graph, constraints)
        if (compareOutput)
          compareOutputGraph(p, historyHandler.graph)
      }
    case PrintCode(nodeId) =>
        logger writeln ("Code :\n" + dg2ast.code(graph, nodeId))

    case ExportGraph(f) =>
      Swing onEDT CSVPrinter(graph, f)

  }


}
