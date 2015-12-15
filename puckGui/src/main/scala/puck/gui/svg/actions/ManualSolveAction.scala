package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.actions.MoveAction
import puck.graph.constraints.DecisionMaker.ChooseNodeKArg
import puck.graph._
import puck.graph.constraints.{AbstractionPolicy, Solver, DecisionMaker}
import puck.graph.transformations.rules.{CreateTypeMember, CreateVarStrategy}
import puck.gui.svg.SVGController
import puck.util.Logged

import scala.swing.Swing.EmptyIcon
import scala.swing.Dialog
import scalaz._, Scalaz._

object ManualSolveAction {


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
                   k : Logged[Option[T]] => Unit,
                   appendNone : Boolean = false) : Unit = {

    choices match {
      case Seq() => k(none[T].set(""))
      case Seq(x) if !appendNone => k(some(x).set(""))
      case _ =>
        val sChoices = choices.map(DisplayableSome(_))
        Dialog.showInput(null, msg, title,
          Dialog.Message.Plain,
          icon = EmptyIcon, if(appendNone) DisplayableNone +: sChoices else sChoices, sChoices.head) match {
          case None => () //Cancel
          case Some(x) => k(x.toOption.set(""))
        }
    }

  }

}

class ManualSolveAction
( violationTarget : ConcreteNode,
  controller : SVGController)
  extends AbstractAction("Solve (manual choices)") with DecisionMaker {

  import controller.graphUtils

  val solver = new Solver(this, graphUtils.transformationRules, false)

  override def actionPerformed(e: ActionEvent): Unit =
    solver.solveViolationsToward(controller.graph.mileStone.set(""), violationTarget){
      puck.actions.printErrOrPushGraph(controller, "Solve Action Error")
    }

  override def violationTarget
  ( lg : LoggedG)
  ( k: Logged[Option[ConcreteNode]] => Unit) : Unit =
    throw new PuckError("should not happen")

  override def abstractionKindAndPolicy
  ( lg : LoggedG, impl : ConcreteNode)
  ( k : Logged[Option[(NodeKind, AbstractionPolicy)]] => Unit) : Unit = {
    ManualSolveAction.forChoice("Abstraction kind an policy",
      s"How to abstract ${lg.value.fullName(impl.id)} ?",
      impl.kind.abstractionChoices, k)
  }

  override def chooseNode
  ( lg : LoggedG, predicate : NodePredicate)
  ( k : ChooseNodeKArg => Unit) : Unit = {

    def k1(sn : Logged[Option[ConcreteNode]]) : Unit ={
        k(sn.map(_.map(n => (lg.value, n.id))))
        //k(Functor[Logged].lift( (sn : Option[ConcreteNode]) => sn.map(n => (graph, n.id))).apply(sn))
    }

    ManualSolveAction.forChoice("Host choice",
      s"${predicate.toString}\n(None will try tro create a new one)",
      lg.value.concreteNodes.filter(predicate(lg.value,_)).toSeq,
          k1, appendNone = true)

  }

  import graphUtils.nodeKindKnowledge.kindOfKindType

  override def createVarStrategy
  ( lg : LoggedG)
  (k :Logged[CreateVarStrategy] => Unit) : Unit = {
    k(MoveAction.getChoice(kindOfKindType(InstanceValueDecl)).
       getOrElse(CreateTypeMember(kindOfKindType(InstanceValueDecl).head)).set(lg.written))
  }


  override def chooseContainerKind
  ( lg : LoggedG, toBeContained : DGNode)
  ( k : Logged[Option[NodeKind]] => Unit) : Unit = {
    val choices = lg.value.nodeKinds.filter(_.canContain(toBeContained.kind))
    ManualSolveAction.forChoice("Host Kind", s"Which kind of container for $toBeContained",
      choices, k)
  }

  override def selectExistingAbstraction
  ( lg : LoggedG, choices : Set[Abstraction])
  ( k : Logged[Option[Abstraction]] => Unit)  : Unit = {
    ManualSolveAction.forChoice("Abstraction Choice",
      s"Use existing abstraction for\n${lg.value.fullName(violationTarget.id)}\n(None will try tro create a new one)",
      choices.toSeq, k, appendNone = true)
  }
}
