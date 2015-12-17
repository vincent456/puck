package puck.gui

import java.io.{PipedInputStream, PipedOutputStream}

import puck.LoadingListener
import puck.graph._
import puck.graph.io._

import puck.gui.svg.SVGFrame
import puck.gui.svg.actions.SwingGraphController
import puck.util.{PuckLogger, PuckLog}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.swing.{Component, ProgressBar, Publisher}
import scala.util.{Failure, Success}

class PuckControl(logger0 : PuckLogger,
                  val filesHandler : FilesHandler,
                  val graphUtils: GraphUtils,
                  private val progressBar : ProgressBar,
                  private val delayedDisplay : ArrayBuffer[Component])
  extends SwingGraphController with Publisher {

  implicit val logger : PuckLogger = logger0
  var dg2ast : DG2AST = _

  def selectedNodes: List[NodeId] = List()

  def selectedEdge: Option[(NodeId, NodeId)] = None

  def initialGraph: DependencyGraph = dg2ast.initialGraph

  import PuckLog.defaultVerbosity


  var displayNameSpaceOnlyDefaultThreshold = 150

  def loadCode( onSuccess : => Unit) = Future {
    progressBar.visible = true
    progressBar.value = 0

    dg2ast = filesHandler.loadGraph(Some(new LoadingListener {
      override def update(loading: Double): Unit =
        progressBar.value = (loading * 100).toInt
    }))
    progressBar.visible = false
    updateStackListeners()

//    if(dg2AST.initialGraph.nodesId.size > displayNameSpaceOnlyDefaultThreshold) {
//      publish(SetTopLevelVisible)
//      logger.writeln(s"Graph have more than $displayNameSpaceOnlyDefaultThreshold, " +
//        s"top level visibility selected by default.")
//        //s"namespace visibility selected by default.")
//    }

    delayedDisplay.foreach(_.visible = true)
  } onComplete {
    case Success(_) => onSuccess
    case Failure(exc) =>
      progressBar.visible = false
      exc.printStackTrace()
  }


//  def explore (trace : Boolean = false,
//               builder : ConstraintSolvingSearchEngineBuilder,
//               automaticConstraintLoosening: Boolean) : Search[DependencyGraph] = {
//
//    val engine = builder(dg2AST.initialGraph, automaticConstraintLoosening)
//
//    puck.util.Time.time(logger, defaultVerbosity) {
//      engine.explore()
//    }
//
//    engine
//  }

  def loadConstraints() : Unit = {
    try {
      logger.writeln("Loading constraints ...")
      dg2ast = filesHandler.parseConstraints(dg2ast)
      updateStackListeners()
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
      new SVGFrame(pipedInput, opts, filesHandler, graphUtils, dg2ast){
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



//  type StateT = SearchState[DependencyGraph]
//  def printStateSeq( subDirStr : String,
//                     states : Seq[StateT],
//                     sPrinter : Option[StateT => String],
//                     printId : Boolean,
//                     printSignature : Boolean,
//                     visibility : VisibilitySet.T): Unit ={
//    val d = filesHandler.graphFile("_results")
//    d.mkdir()
//    val subDir = filesHandler.graphFile("_results%c%s".format(File.separatorChar, subDirStr))
//    subDir.mkdir()
//    filesHandler.printCSSearchStatesGraph(subDir, states, graphUtils.dotHelper, visibility, sPrinter, printId, printSignature)
//  }
//
//  def showStateSeq(states : Seq[StateT],
//                   printId : Boolean,
//                   printSignature : Boolean,
//                   visibility : VisibilitySet.T): Unit = {
//    Future(new ImageExplorer(filesHandler, graphUtils.dotHelper, logger, states.toIndexedSeq, visibility, printId, printSignature))
//    ()
//  }

  reactions += {
    case LoadCodeRequest => loadCode(loadConstraints())

    case LoadConstraintRequest => loadConstraints()

    case GraphDisplayRequest(title, graph, printId, printSignature, visibility, sUse, format) =>
      displayGraph(title, graph,
        PrintingOptions(visibility, printId, printSignature, sUse))(format)

    case ConstraintDisplayRequest(graph) =>
      graph.printConstraints(logger, defaultVerbosity)

    case ApplyOnCodeRequest(searchResult) => applyOnCode(searchResult)

//
//      val engine = builder(dg2AST.initialGraph,
//                          automaticConstraintLoosening = true)
//
//      Future {
//        logger.writeln("Solving constraints ...")
//        puck.util.Time.time(logger, defaultVerbosity) {
//          engine.explore()
//        }
//        engine
//      } onComplete {
//        case Success(res) =>
//          logger.writeln("Solving done")
//          publish(ExplorationFinished(res))
//        case Failure(exc) =>
//          logger.writeln("Solving failure")
//          exc.printStackTrace()
//          publish(ExplorationFinished(engine))
//          //filesHandler.logger writeln exc.getStackTrace.mkString("\n")
//      }

//    case SearchStateMapPrintingRequest(stateMap, printId, printSignature, visibility) =>
//      filesHandler.printCSSearchStatesGraph(stateMap, graphUtils.dotHelper, visibility, printId, printSignature)
//
//    case SearchStateSeqPrintingRequest(subDir, states, sPrinter, printId, printSignature, visibility) =>
//     // printStateSeq(subDir, states, sPrinter, printId, printSignature)
//      logger.writeln("history request")
//      showStateSeq(states, printId, printSignature, visibility)
  }


}
