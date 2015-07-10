package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph.LoggedError
import puck.graph.constraints.RedirectionPolicy
import puck.graph.transformations.rules.Redirection
import puck.graph.{ShowDG, NodeId, DGUses, ConcreteNode}
import puck.gui.svg.SVGController

import ShowDG._

case class RedirectAction
( newTarget : ConcreteNode,
  edge : DGUses,
  policy : RedirectionPolicy,
  controller: SVGController)
  extends AbstractAction(s"Use this ($policy) instead of ${showDG[NodeId](controller.graph).shows(edge.target)}"){

  //TODO check keepOldUse and propagate redirection value
  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Redirection Action failure"){
      val absSet = controller.graph.abstractions(edge.used).filter {
        abs => abs.nodes.contains(newTarget.id)
      }

      if(absSet.size != 1)
        LoggedError(new PuckError(),
          s"$newTarget is registered ${absSet.size} times as ${edge.used} abstraction")
      else
        Redirection.redirectUsesAndPropagate(controller.graph.mileStone,
          edge, absSet.head)

    }


}
