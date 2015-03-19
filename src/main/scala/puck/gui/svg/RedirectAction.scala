package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.constraints.RedirectionPolicy
import puck.graph.{DGEdge, ConcreteNode}

import scalaz.{Success, Failure}

/**
 * Created by lorilan on 3/19/15.
 */
case class RedirectAction
( newTarget : ConcreteNode,
  edge : DGEdge,
  policy : RedirectionPolicy,
  controller: SVGController)
  extends AbstractAction(s"Redirect selected edge toward this ($policy)"){

  //TODO check keepOldUse and propagate redirection value
  override def actionPerformed(e: ActionEvent): Unit =
    controller.transfoRules.
      redirectUsesOf(controller.getGraph, edge, newTarget.id, policy) match {
      case Failure(errs) =>
        controller.console.appendText("Abstraction creation failure\n" )
        errs.foreach(e => controller.console.appendText(e.getMessage + "\n"))
      case Success((_, g)) => controller.pushGraph(g)
    }
}
