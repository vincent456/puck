package puck.gui

import java.io.{PipedInputStream, PipedOutputStream}

import puck.{StackListener, GraphStack, LoadingListener}
import puck.graph._
import puck.graph.io._

import puck.gui.svg.SVGFrame
import puck.util.{PuckLogger, PuckLog}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.swing.{Component, ProgressBar, Publisher}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
class PuckControl(logger0 : PuckLogger,
                  val filesHandler : FilesHandler,
                  val graphUtils: GraphUtils,
                  private val progressBar : ProgressBar,
                  private val delayedDisplay : ArrayBuffer[Component])
  extends Publisher with StackListener{

  implicit val logger : PuckLogger = logger0
  var dg2ast : DG2AST = _
  var graphStack : GraphStack = _
  def graph : DependencyGraph = graphStack.graph

  import PuckLog.defaultVerbosity

  def update(controller: GraphStack): Unit =
    publish(GraphUpdate(controller.graph))

  def loadCode( onSuccess : => Unit) = Future {
    progressBar.visible = true
    progressBar.value = 0

    dg2ast = filesHandler.loadGraph(Some(new LoadingListener {
      override def update(loading: Double): Unit =
        progressBar.value = (loading * 100).toInt
    }))
    progressBar.visible = false

    graphStack = new GraphStack {
      implicit def logger: PuckLogger = logger0
      def initialGraph: DependencyGraph = dg2ast.initialGraph
    }
    graphStack.registerAsStackListeners(this)

    delayedDisplay.foreach(_.visible = true)
  } onComplete {
    case Success(_) => onSuccess
    case Failure(exc) =>
      progressBar.visible = false
      exc.printStackTrace()
  }


  def loadConstraints() : Unit = {
    try {
      logger.writeln("Loading constraints ...")
      dg2ast = filesHandler.parseConstraints(dg2ast)
      graphStack.updateStackListeners()
      logger.writeln(" done:")
      dg2ast.initialGraph.printConstraints(logger, defaultVerbosity)
    }
    catch {
      case _ : java.io.FileNotFoundException => logger writeln "constraint file not found"
      case e: Error => logger writeln e.getMessage
    }
  }

  def displayGraph(title : String,
                   graph : DependencyGraph,
                   opts : PrintingOptions) : DotOutputFormat => Unit = { format =>
    logger.writeln("Printing graph ...")

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)


    Future {
      logger.writeln("requesting svg frame")
      new SVGFrame(pipedInput, opts, filesHandler, graphUtils, dg2ast, graphStack){
        this.setTitle(title)
      }
    }

    DotPrinter.genImage(graph, graphUtils.dotHelper, opts, format, pipedOutput) {
      case Success(i) if i == 0 => logger.writeln("success")
      case _ => logger.writeln("fail")
    }
  }

  def applyOnCode(record : DependencyGraph) : Unit =
    Future {
      logger.write("generating code ...")
      dg2ast(record)
      dg2ast.printCode(filesHandler.outDirectory !)
      logger.writeln(" done")
    } onComplete {
      case Success(_) => ()
      case Failure(exc) => exc.printStackTrace()
    }

  reactions += {
    case LoadCodeRequest => loadCode(loadConstraints())

    case LoadConstraintRequest => loadConstraints()

    case GraphDisplayRequest(title, graph, printId, printSignature, visibility, sUse, format) =>
      displayGraph(title, graph,
        PrintingOptions(visibility, printId, printSignature, sUse))(format)

    case ConstraintDisplayRequest(graph) =>
      graph.printConstraints(logger, defaultVerbosity)

    case ApplyOnCodeRequest(searchResult) => applyOnCode(searchResult)

  }


}
