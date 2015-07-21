package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph._
import puck.graph.constraints.RedirectionPolicy
import puck.graph.transformations.rules.Redirection
import puck.gui.svg.SVGController

import ShowDG._

case class RedirectAction
( newTarget : ConcreteNode,
  edge : DGUses,
  abs : Abstraction,
  controller: SVGController)
  extends AbstractAction(s"Use $abs instead of ${showDG[NodeId](controller.graph).shows(edge.target)}"){

  //TODO check keepOldUse and propagate redirection value
  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Redirection Action failure"){
        Redirection.redirectUsesAndPropagate(controller.graph.mileStone,
          edge, abs)

    }


}
