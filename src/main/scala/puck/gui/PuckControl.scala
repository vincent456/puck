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
case class GraphDisplayRequest[Kind <: NodeKind[Kind], T](title : String,
                                                       graph : AccessGraph[Kind, T],
                                                       sUse : Option[AGEdge[Kind]] = None) extends ControlRequest

case class ExploreRequest[Kind <: NodeKind[Kind],T](trace : Boolean,
                                                  builder : ConstraintSolvingSearchEngineBuilder[Kind, T]) extends ControlRequest

case class SearchStateMapPrintingRequest[Kind <: NodeKind[Kind], T](stateMap : Map[Int, Seq[SearchState[ResultT[Kind, T],_]]])
  extends ControlRequest
case class SearchStateSeqPrintingRequest[Kind <: NodeKind[Kind], T](subDir : String,
                                                                  states : Seq[SearchState[ResultT[Kind, T],_]],
                                                                  sPrinter : Option[SearchState[ResultT[Kind,T],_] => String])
  extends ControlRequest

case class PrintConstraintRequest() extends ControlRequest
case class ApplyOnCodeRequest[Kind <: NodeKind[Kind], T](searchResult : ResultT[Kind, T]) extends ControlRequest

/*
case class SolveRequest[Kind <: NodeKind[Kind]](decisionStrategy : DecisionMaker[Kind],
                                                trace : Boolean) extends ControlRequest
case class DoWholeProcessRequest(trace : Boolean) extends ControlRequest

*/
sealed abstract class Answer extends Event
case class ExplorationFinished[Kind <: NodeKind[Kind], T](result : Search[ResultT[Kind, T]]) extends Answer


class PuckControl[Kind <: NodeKind[Kind], T](val filesHandler : FilesHandler[Kind, T],
                                          private val progressBar : ProgressBar,
                                          private val delayedDisplay : ArrayBuffer[Component])
  extends Publisher{

  type GraphT = AccessGraph[Kind, T]
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
                   someUse : Option[AGEdge[Kind]] = None){

   /* sRecording match {
      case Some(rec) =>
        rec()
        PuckControl.this.publish(AccessGraphModified(rec.graph))

      case None => ()
    }*/

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

  /*def applyOnCode(record : Recording[Kind]){
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
*/
  type StateT = SearchState[ResultT[Kind, T],_]
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
        sUse.asInstanceOf[Option[AGEdge[Kind]]])

    case ApplyOnCodeRequest(record) => ???
      //applyOnCode(record.asInstanceOf[Recording[Kind]])

  /*  case SolveRequest(dm, trace) =>
      doSolve(dm.asInstanceOf[DecisionMaker[Kind]], trace){
        filesHandler.logger.writeln("Solving done")
      }*/

    case ExploreRequest(trace, builder) =>

      val tbuilder = builder.asInstanceOf[ConstraintSolvingSearchEngineBuilder[Kind,T]]

      val engine = tbuilder(filesHandler.graph)

      Future {
        filesHandler.logger.writeln("Solving constraints ...")
        puck.util.Time.time(filesHandler.logger, defaultVerbosity) {
          engine.search()
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

    case req @ SearchStateMapPrintingRequest(_) =>
      val treq = req.asInstanceOf[SearchStateMapPrintingRequest[Kind, T]]
      filesHandler.printCSSearchStatesGraph(treq.stateMap)

    case req @ SearchStateSeqPrintingRequest(_, _, _) =>
       val treq = req.asInstanceOf[SearchStateSeqPrintingRequest[Kind, T]]
      printStateSeq(treq.subDir, treq.states, treq.sPrinter)
  }

}
