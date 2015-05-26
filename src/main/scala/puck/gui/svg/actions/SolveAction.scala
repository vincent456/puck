package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph.constraints.{NodePredicate, AbstractionPolicy, DecisionMaker}
import puck.graph._
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.Field

import scala.swing.Swing.EmptyIcon
import scala.swing.Dialog

object SolveAction {


  sealed abstract class DisplayableChoice[+A]{
    def toOption : Option[A]
  }
  case object DisplayableNone extends DisplayableChoice[Nothing]{
    override def toString = "None of the choices above"
    val toOption = None

  }
  case class DisplayableSome[T](value : T) extends DisplayableChoice[T]{
    override def toString = value.toString
    def toOption = Some(value)
  }
  

  def forChoice[T](title : String,
                   msg : Any,
                   choices : Seq[T],
                   k : Option[T] => Unit,
                   appendNone : Boolean = false) : Unit = {

    choices match {
      case Seq() => k(None)
      case Seq(x) if !appendNone => k(Some(x))
      case _ =>
        val sChoices = choices.map(DisplayableSome(_))
        Dialog.showInput(null, msg, title,
          Dialog.Message.Plain,
          icon = EmptyIcon, if(appendNone) DisplayableNone +: sChoices else sChoices, sChoices.head) match {
          case None => () //Cancel
          case Some(x) => k(x.toOption)
        }
    }
  }

}

class SolveAction
( violationTarget : ConcreteNode,
  controller : SVGController)
  extends AbstractAction("Solve") with DecisionMaker {

  val solver = controller.solverBuilder(this, false)

  override def actionPerformed(e: ActionEvent): Unit =
    solver.solveViolationsToward(controller.graph.mileStone, violationTarget){
      printErrOrPushGraph(controller, "Solve Action Error")
    }

  override def violationTarget(graph: DependencyGraph)
                              (k: (Option[ConcreteNode]) => Unit): Unit =
    throw new PuckError("should not happen")

  override def abstractionKindAndPolicy(graph: DependencyGraph, impl: ConcreteNode)
                                       (k: (Option[(NodeKind, AbstractionPolicy)]) => Unit): Unit = {
    SolveAction.forChoice("Abstraction kind an policy",
      s"How to abstract ${graph.fullName(impl.id)} ?",
      impl.kind.abstractionChoices, k)
  }

  override def chooseNode(graph: DependencyGraph,
                          predicate: NodePredicate)
                         (k: (DependencyGraph) => (Option[NodeId]) => Unit): Unit = {
    SolveAction.forChoice("Host choice", s"${predicate.toString}\n(None will try tro create a new one)",
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
    SolveAction.forChoice("Host Kind", s"Which kind of container for $toBeContained",
      choices, k)
  }

  override def selectExistingAbstraction
  ( graph: DependencyGraph,
    choices: Set[(NodeId, AbstractionPolicy)])
  ( k: (Option[(NodeId, AbstractionPolicy)]) => Unit): Unit = {
    SolveAction.forChoice("Abstraction Choice",
      s"Use existing abstraction for\n${graph.fullName(violationTarget.id)}\n(None will try tro create a new one)",
      choices.toSeq, k, appendNone = true)
  }
}
