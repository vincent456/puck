package puck.gui

import java.io.{PipedInputStream, PipedOutputStream}

import AST.LoadingListener
import puck.graph.constraints.DecisionMaker
import puck.graph.{AGEdge, FilesHandler, NodeKind}
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

case class LoadConstraintRequest() extends ControlRequest
case class GraphDisplayRequest[Kind <: NodeKind[Kind]](sUse : Option[AGEdge[Kind]] = None) extends ControlRequest
case class ApplyOnCodeRequest() extends ControlRequest
case class LoadCodeRequest() extends ControlRequest
case class SolveRequest[Kind <: NodeKind[Kind]](decisionStrategy : DecisionMaker[Kind],
                                                trace : Boolean) extends ControlRequest
case class ExploreRequest(trace : Boolean) extends ControlRequest
case class DoWholeProcessRequest(trace : Boolean) extends ControlRequest
case class PrintConstraintRequest() extends ControlRequest


class PuckControl[Kind <: NodeKind[Kind]](val filesHandler : FilesHandler[Kind],
                                          private val progressBar : ProgressBar,
                                          private val delayedDisplay : ArrayBuffer[Component])
  extends Publisher{

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
      filesHandler.logger.write("Loading constraints ...")
      filesHandler.graph.discardConstraints()
      filesHandler.parseConstraints()
      filesHandler.logger.writeln(" done:")
      filesHandler.graph.printConstraints(filesHandler.logger)
    }
    catch {
      case e: Error => println("\n" + e.getMessage)
    }
  }

  def displayGraph(someUse : Option[AGEdge[Kind]] = None){
    filesHandler.logger.write("Printing graph ...")

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    Future {
      val imgframe = ImageFrame(pipedInput)
      imgframe.visible = true
    }

    filesHandler.makePng(sOutput = Some(pipedOutput),
                         selectedUse = someUse){
      case Success(i) if i == 0 =>
        filesHandler.logger.writeln("success")
      case _ => filesHandler.logger.writeln("fail")
    }
  }

  def doSolve(decisionMaker : DecisionMaker[Kind],
              trace : Boolean)
             (onSuccess : => Unit ) =
    Future {
      filesHandler.logger.writeln("Solving constraints ...")
      filesHandler.solve(trace, decisionMaker)
    } onComplete {
      case Success(_) =>
        publish(AccessGraphModified(filesHandler.graph))
        onSuccess
      case Failure(exc) => println(exc.printStackTrace())
    }

  def applyOnCode(){
    Future {
      filesHandler.logger.write("generating code ...")
      filesHandler.graph.applyChangeOnProgram()
      filesHandler.printCode()
      filesHandler.logger.writeln(" done")
    }
  }



  reactions += {
    case LoadCodeRequest() => loadCode(loadConstraints())

    case LoadConstraintRequest() => loadConstraints()

    case GraphDisplayRequest(sUse) =>
      println("request received")
      displayGraph(sUse.asInstanceOf[Option[AGEdge[Kind]]])

    case ApplyOnCodeRequest() => applyOnCode()

    case SolveRequest(dm, trace) =>
      doSolve(dm.asInstanceOf[DecisionMaker[Kind]], trace){
        filesHandler.logger.writeln("Solving done")
      }

    case ExploreRequest(trace) =>
      Future {
        filesHandler.logger.writeln("Solving constraints ...")
        filesHandler.explore(trace)
      } onComplete {
        case Success(_) => filesHandler.logger.writeln("Solving done")
        case Failure(exc) => println(exc.printStackTrace())
      }

    case DoWholeProcessRequest(trace) =>
      loadCode {
        loadConstraints()
        displayGraph()
        doSolve(filesHandler.decisionMaker(), trace){
          displayGraph()
          applyOnCode()
          filesHandler.openSources()
          filesHandler.openProduction()
        }
      }
  }

}
