package puck.gui

import java.io.{File, PipedInputStream, PipedOutputStream}

import AST.LoadingListener
import puck.graph.io.ConstraintSolvingSearchEngineBuilder
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

sealed abstract class ControlRequest extends Event

case class LoadCodeRequest() extends ControlRequest
case class LoadConstraintRequest() extends ControlRequest
case class GraphDisplayRequest
(title : String,
 graph : AccessGraph,
 sUse : Option[AGEdge] = None)
  extends ControlRequest

case class ExploreRequest
(trace : Boolean,
 builder : ConstraintSolvingSearchEngineBuilder)
  extends ControlRequest

case class SearchStateMapPrintingRequest(stateMap : Map[Int, Seq[SearchState[ResultT]]])
  extends ControlRequest
case class SearchStateSeqPrintingRequest
(subDir : String,
 states : Seq[SearchState[ResultT]],
 sPrinter : Option[SearchState[ResultT] => String])
  extends ControlRequest

case class PrintConstraintRequest() extends ControlRequest
case class ApplyOnCodeRequest(searchResult : ResultT) extends ControlRequest

sealed abstract class Answer extends Event
case class ExplorationFinished(result : Search[ResultT]) extends Answer


class PuckControl(val filesHandler : FilesHandler,
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
      logger.write("Loading constraints ...")
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
                   someUse : Option[AGEdge] = None){

    logger.write("Printing graph ...")

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    Future {
      val imgframe = ImageFrame(pipedInput)
      imgframe.title = title
      imgframe.visible = true
    }

    filesHandler.makePng(graph,
                         printId = true,
                         sOutput = Some(pipedOutput),
                         selectedUse = someUse){
      case Success(i) if i == 0 => logger.writeln("success")
      case _ => logger.writeln("fail")
    }
  }

  def applyOnCode(record : Recording){
    Future {
      filesHandler.logger.write("generating code ...")
      filesHandler.graph.applyChangeOnProgram(record)
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
                     sPrinter : Option[StateT => String]): Unit ={
    val d = filesHandler.graphFile("_results")
    d.mkdir()
    val subDir = filesHandler.graphFile("_results%c%s".format(File.separatorChar, subDirStr))
    subDir.mkdir()
    filesHandler.printCSSearchStatesGraph(subDir, states, sPrinter)
  }

  reactions += {
    case LoadCodeRequest() => loadCode(loadConstraints())

    case LoadConstraintRequest() => loadConstraints()

    case GraphDisplayRequest(title, graph, sUse) =>
      displayGraph(title,
        graph.asInstanceOf[GraphT],
        sUse.asInstanceOf[Option[AGEdge]])

    case ApplyOnCodeRequest(record) => ???
      //applyOnCode(record.asInstanceOf[Recording[Kind]])



    case ExploreRequest(trace, builder) =>

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

    case treq @ SearchStateMapPrintingRequest(_) =>
      filesHandler.printCSSearchStatesGraph(treq.stateMap)

    case treq @ SearchStateSeqPrintingRequest(_, _, _) =>
      printStateSeq(treq.subDir, treq.states, treq.sPrinter)
  }

}
