package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{JOptionPane, AbstractAction}

import puck.PuckError
import puck.graph.constraints.{NodePredicate, AbstractionPolicy, DecisionMaker}
import puck.graph._
import puck.graph.transformations.{CreateTypeMember, CreateVarStrategy}
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.Field

import scala.swing.Swing.EmptyIcon
import scalaz.{Failure, Success}
import scala.swing.Dialog

/**
 * Created by lorilan on 4/1/15.
 */

object SolveAction {

 /* //TODO make difference between cancel and choose nothing
  def choiceDialog[T]( title : String,
                       msg : String,
                       choicesArray : Seq[T]) : Option[Option[T]] = {


    if(choicesArray.isEmpty) Some(None)
    else {
      val choice = JOptionPane.showInputDialog(null, //Component parentComponent
        JOptionPane.PLAIN_MESSAGE, //int messageType
        null, //Icon icon,
        choicesArray.asInstanceOf[Array[Object]], //Object[] options,
        choicesArray(0) //Object initialValue
      ).asInstanceOf[T]
      if (choice == null) None
      else Some(Some(choice))
    }
  }*/

  def forChoice[T](title : String,
                   msg : Any,
                   choices : Seq[T],
                   k : Option[T] => Unit,
                   appendNone : Boolean = false) : Unit = {

    choices match {
      case Seq() => k(None)
      case Seq(x) if !appendNone => k(Some(x))
      case _ =>
        val sChoices = choices.map(Some(_))
        Dialog.showInput(null, msg, title,
          Dialog.Message.Plain,
          icon = EmptyIcon, if(appendNone) None +: sChoices else sChoices, sChoices.head) match {
          case None => () //Cancel
          case Some(x) => k(x)
        }
    }
  }

}

class SolveAction
( violationTarget : ConcreteNode,
  controller : SVGController)
  extends AbstractAction("Solve") with DecisionMaker {

  val solver = controller.solverBuilder(this,
    automaticConstraintLoosening = false)

  override def actionPerformed(e: ActionEvent): Unit = {
    solver.solveViolationsToward(controller.graph, violationTarget){
      case Success(g) => controller.pushGraph(g)
      case Failure(errs) =>
        errs.foreach(err => controller.console.appendText(err.getMessage))
    }
  }

  override def violationTarget(graph: DependencyGraph)(k: (Option[ConcreteNode]) => Unit): Unit =
    throw new PuckError("should not happen")

  override def abstractionKindAndPolicy(graph: DependencyGraph, impl: ConcreteNode)
                                       (k: (Option[(NodeKind, AbstractionPolicy)]) => Unit): Unit = {
    SolveAction.forChoice("Abstraction kind an policy",
      "How to abstract this node ?",
      impl.kind.abstractionChoices, k)
  }

  override def chooseNode(graph: DependencyGraph,
                          predicate: NodePredicate)
                         (k: (DependencyGraph) => (Option[NodeId]) => Unit): Unit = {
    SolveAction.forChoice("Host choice", predicate,
          graph.concreteNodes.filter(predicate(graph,_)).toSeq,
          (sn : Option[ConcreteNode]) => k(graph)(sn.map(_.id)), appendNone = true)

  }

  override def createVarStrategy(k : CreateVarStrategy => Unit) : Unit = {
    k(MoveAction.getChoice(Field).
       getOrElse(CreateTypeMember(Field)))
  }


  override def chooseContainerKind(graph: DependencyGraph, toBeContained: DGNode)
                                  (k: (Option[NodeKind]) => Unit): Unit = {
    val choices = graph.nodeKinds.filter(_.canContain(toBeContained.kind))
    SolveAction.forChoice("Host Kind", s"Which kind of node to contain $toBeContained",
      choices, k)
  }
}
