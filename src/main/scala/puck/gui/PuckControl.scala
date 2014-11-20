package puck.gui

import java.io.{File, PipedInputStream, PipedOutputStream}

import AST.LoadingListener
import puck.graph.io.{VisibilitySet, ConstraintSolvingSearchEngineBuilder}
import puck.gui.explorer.AccessGraphModified
import puck.gui.imageDisplay.{ImageFrame, ImageExplorer}
import puck.search.{SearchState, Search}

/*
import puck.graph.mutable.backTrack.Recording
import puck.graph.mutable.constraints.DecisionMaker
*/
import puck.graph._
import puck.util.PuckLog
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.event.Event
import scala.swing.{Component, ProgressBar, Publisher}
import scala.util.{Failure, Success}


/**
 * Created by lorilan on 11/08/14.
 */

sealed trait ControlRequest extends Event

case class LoadCodeRequest() extends ControlRequest
case class LoadConstraintRequest() extends ControlRequest
case class GraphDisplayRequest
(title : String,
 graph : AccessGraph,
 printId : Boolean,
 printSignature : Boolean,
 sUse : Option[AGEdge] = None)
 extends ControlRequest

case class ConstraintDisplayRequest(graph : AccessGraph) extends ControlRequest
case class ExploreRequest
(builder : ConstraintSolvingSearchEngineBuilder)
  extends ControlRequest

case class SearchStateMapPrintingRequest
(stateMap : Map[Int, Seq[SearchState[ResultT]]],
 printId : Boolean,
 printSignature : Boolean)
  extends ControlRequest
case class SearchStateSeqPrintingRequest
(subDir : String,
 states : Seq[SearchState[ResultT]],
 sPrinter : Option[SearchState[ResultT] => String],
 printId : Boolean,
 printSignature : Boolean)
  extends ControlRequest

case class PrintConstraintRequest() extends ControlRequest
case class ApplyOnCodeRequest(searchResult : ResultT) extends ControlRequest

sealed abstract class Answer extends Event
case class ExplorationFinished(result : Search[ResultT]) extends Answer


class PuckControl(val filesHandler : FilesHandler,
                  private val visibility : VisibilitySet,
                  private val progressBar : ProgressBar,
                  private val delayedDisplay : ArrayBuffer[Component])
  extends Publisher{

  type GraphT = AccessGraph
  import PuckLog.defaultVerbosity

  import filesHandler.logger

  def loadCode( onSuccess : => Unit) = Future {
    progressBar.visible = true
    progressBar.value = 0

    val ag = filesHandler.loadGraph(new LoadingListener {
      override def update(loading: Double): Unit =
        progressBar.value = (loading * 100).toInt
    })
    progressBar.visible = false
    publish(AccessGraphModified(ag))

    delayedDisplay.foreach(_.visible = true)
  } onComplete {
    case Success(_) => onSuccess
    case Failure(exc) =>
      progressBar.visible = false
      exc.printStackTrace()
  }


  def loadConstraints(){
    try {
      logger.writeln("Loading constraints ...")
      filesHandler.parseConstraints()
      logger.writeln(" done:")
      filesHandler.graph.printConstraints(logger, defaultVerbosity)
    }
    catch {
      case e: Error => logger writeln ("\n" + e.getMessage)
    }
  }

  def displayGraph(title : String,
                   graph : GraphT,
                   someUse : Option[AGEdge],
                   printId : Boolean,
                   printSignature : Boolean){

    logger.writeln("Printing graph ...")

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    Future {
      val imgframe = ImageFrame(pipedInput)
      imgframe.title = title
    }

    filesHandler.makePng(graph, visibility, printId, printSignature,
                         someUse, sOutput = Some(pipedOutput)){
      case Success(i) if i == 0 => logger.writeln("success")
      case _ => logger.writeln("fail")
    }
  }

  def applyOnCode(record : ResultT){
    Future {
      filesHandler.logger.write("generating code ...")
      filesHandler.applyChangeOnProgram(record)
      filesHandler.printCode()
      filesHandler.logger.writeln(" done")
    } onComplete {
      case Success(_) => ()
      case Failure(exc) => exc.printStackTrace()
    }
  }

  type StateT = SearchState[ResultT]
  def printStateSeq( subDirStr : String,
                     states : Seq[StateT],
                     sPrinter : Option[StateT => String],
                     printId : Boolean,
                     printSignature : Boolean): Unit ={
    val d = filesHandler.graphFile("_results")
    d.mkdir()
    val subDir = filesHandler.graphFile("_results%c%s".format(File.separatorChar, subDirStr))
    subDir.mkdir()
    filesHandler.printCSSearchStatesGraph(subDir, states, visibility, sPrinter, printId, printSignature)
  }

  def showStateSeq(states : Seq[StateT],
                   printId : Boolean,
                   printSignature : Boolean): Unit = {
    Future {
      new ImageExplorer(filesHandler, states.toIndexedSeq, visibility, printId, printSignature)
    }
  }

  reactions += {
    case LoadCodeRequest() => loadCode(loadConstraints())

    case LoadConstraintRequest() => loadConstraints()

    case GraphDisplayRequest(title, graph, printId, printSignature, sUse) =>
      displayGraph(title, graph, sUse, printId, printSignature)

    case ConstraintDisplayRequest(graph) =>
      graph.printConstraints(filesHandler.logger, defaultVerbosity)

    case ApplyOnCodeRequest(searchResult) => applyOnCode(searchResult)

    case ExploreRequest(builder) =>

      val engine = builder(filesHandler.initialRecord,
                           filesHandler.graph)

      Future {
        filesHandler.logger.writeln("Solving constraints ...")
        puck.util.Time.time(filesHandler.logger, defaultVerbosity) {
          engine.explore()
        }
        engine
      } onComplete {
        case Success(res) =>
          filesHandler.logger.writeln("Solving done")
          publish(ExplorationFinished(res))
        case Failure(exc) =>
          filesHandler.logger.writeln("Solving failure")
          exc.printStackTrace()
          publish(ExplorationFinished(engine))
          //filesHandler.logger writeln exc.getStackTrace.mkString("\n")
      }

    case SearchStateMapPrintingRequest(stateMap, printId, printSignature) =>
      filesHandler.printCSSearchStatesGraph(stateMap, visibility, printId, printSignature)

    case SearchStateSeqPrintingRequest(subDir, states, sPrinter, printId, printSignature) =>
     // printStateSeq(subDir, states, sPrinter, printId, printSignature)
      logger.writeln("history request")
      showStateSeq(states, printId, printSignature)
  }

}
