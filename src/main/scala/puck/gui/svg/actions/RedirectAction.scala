package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.constraints.RedirectionPolicy
import puck.graph.transformations.rules.Redirection
import puck.graph.{ConcreteNode, DGEdge}
import puck.gui.svg.SVGController

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
    printErrOrPushGraph(controller,"Redirection Action failure"){
      Redirection.redirectUsesAndPropagate(controller.graph, edge, newTarget.id, policy)
    }


}
