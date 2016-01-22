package puck.gui

import java.io.{File, PipedInputStream, PipedOutputStream}

import puck.{FilesHandlerDG2ASTControllerOps, StackListener, GraphStack, LoadingListener}
import puck.graph._
import puck.graph.io._

import puck.gui.svg.SVGFrame
import puck.util.{PuckLogger, PuckLog}

import scala.concurrent.Future
import scala.swing.{ProgressBar, Publisher}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.{\/-, -\/}



class PuckControl
( logger0 : PuckLogger,
  val filesHandler : FilesHandler,
  val graphUtils: GraphUtils,
  private val progressBar : ProgressBar)
  extends Publisher
  with StackListener {

  implicit val logger : PuckLogger = logger0
  var dg2ast : DG2AST = _
  var graphStack : GraphStack = _
  def graph : DependencyGraph = graphStack.graph

  import PuckLog.defaultVerbosity

  this deafTo this

  private [this] var printingOptionsControl0 : PrintingOptionsControl = _

  def printingOptionsControl = printingOptionsControl0

  def update(graphStack: GraphStack): Unit =
    publish(GraphUpdate(graphStack.graph))


  def loadCodeAndConstraints() = Future {
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
    printingOptionsControl0 = PrintingOptionsControl(graphStack.graph)
    graphStack.registerAsStackListeners(this)

  } onComplete {
    case Success(_) => loadConstraints(updateAnyway = true)
    case Failure(exc) =>
      progressBar.visible = false
      exc.printStackTrace()
  }


  def loadConstraints(updateAnyway : Boolean = false) : Unit = {
    try {
      logger.writeln("Loading constraints ...")
      dg2ast = filesHandler.parseConstraints(dg2ast)
      graphStack.updateStackListeners()
      logger.writeln(" done:")
      dg2ast.initialGraph.printConstraints(logger, defaultVerbosity)
    }
    catch {
      case e: Error =>
        logger writeln e.getMessage
        if(updateAnyway) {
          graphStack.updateStackListeners()
        }
    }

  }

  def displayGraph(title : String,
                   graph : DependencyGraph,
                   visibility : VisibilitySet.T,
                   sUse : Option[Uses]) : Unit = {
    logger.writeln("Printing graph ...")

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    printingOptionsControl.visibility = visibility
    printingOptionsControl.selectedEdgeForTypePrinting = sUse

    val opts = printingOptionsControl.printingOptions

    Future {
      logger.writeln("requesting svg frame")
      new SVGFrame(pipedInput, this){
        this.setTitle(title)
      }
    }

    DotPrinter.genImage(graph, graphUtils.dotHelper, opts, Svg, pipedOutput) {
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

  def saveRecordOnFile(file : File) : Unit = {
    Recording.write(file.getAbsolutePath, dg2ast.nodesByName, graph)
  }

  def loadRecord(file : File) : Unit = {
    try graphStack.load(Recording.load(file.getAbsolutePath, dg2ast.nodesByName))
    catch {
      case Recording.LoadError(msg, m) =>
        logger writeln ("Record loading error " + msg)
        logger writeln ("cannot bind loaded map " + m.toList.sortBy(_._1).mkString("\n"))
        logger writeln ("with " + dg2ast.nodesByName.toList.sortBy(_._1).mkString("\n"))
    }

  }


  def publishUndoRedoStatus() : Unit =
   this publish UndoRedoStatus(graphStack.canUndo, graphStack.canRedo)


  reactions += {

    case PushGraph(g) => graphStack.pushGraph(g)

    case PrintErrOrPushGraph(msg, lgt) =>
      lgt.value match {
        case -\/(err) =>
          logger.writeln(s"$msg\n${err.getMessage}\nLog : ${lgt.log}")
        case \/-(g) =>
          logger.writeln(lgt.log)
          graphStack.pushGraph(g)
      }

    case RewriteHistory(r) =>
      graphStack.rewriteHistory(r)

    case Log(msg) =>
      logger.writeln(msg)

    case UndoAll =>
      graphStack.undoAll()
      publishUndoRedoStatus()

    case Undo =>
      graphStack.undo()
      publishUndoRedoStatus()

    case Redo =>
      graphStack.redo()
      publishUndoRedoStatus()

    case gf @ GraphFocus(g, e) =>
      printingOptionsControl.focus(g, e)
      this publish gf

    case LoadCodeRequest => loadCodeAndConstraints()

    case LoadConstraintRequest => loadConstraints()

    case SaveRecord(f) =>
      saveRecordOnFile(f)
    case LoadRecord(f) =>
      loadRecord(f)
      this publish GraphUpdate(graph)

    case GraphDisplayRequest(title, graph, visibility, sUse) =>
      displayGraph(title, graph, visibility, sUse)

    case ConstraintDisplayRequest(graph) =>
      graph.printConstraints(logger, defaultVerbosity)

    case ApplyOnCodeRequest(searchResult) => applyOnCode(searchResult)

    case PrintingOptionsUpdate => this publish GraphUpdate(graphStack.graph)
    case pe : PrintingOptionEvent => pe(printingOptionsControl)

    case GenCode(compareOutput) =>
      import FilesHandlerDG2ASTControllerOps._
      deleteOutDirAndapplyOnCode(dg2ast, filesHandler, graphStack.graph)
      if(compareOutput)
        compareOutputGraph(filesHandler, graphStack.graph)



    case evt => publish(evt)


  }


}
