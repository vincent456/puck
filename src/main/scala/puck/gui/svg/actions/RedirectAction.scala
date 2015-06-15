package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.constraints.RedirectionPolicy
import puck.graph.transformations.rules.Redirection
import puck.graph.{NodeId, DGUses, ConcreteNode}
import puck.gui.svg.SVGController
import puck.graph.ShowDG._
case class RedirectAction
( newTarget : ConcreteNode,
  edge : DGUses,
  policy : RedirectionPolicy,
  controller: SVGController)
  extends AbstractAction(s"Use this ($policy) instead of ${showDG[NodeId](controller.graph).shows(edge.target)}"){

  //TODO check keepOldUse and propagate redirection value
  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Redirection Action failure"){
      Redirection.redirectUsesAndPropagate(controller.graph.mileStone, edge, newTarget.id, policy,
        propagateRedirection = true, keepOldUse = false)
    }


}
