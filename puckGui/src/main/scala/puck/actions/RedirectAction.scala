package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.transformations.rules.Redirection
import puck.gui.svg.SVGController

case class RedirectAction
( controller : UtilGraphStack,
  newTarget : ConcreteNode,
  edge : Uses,
  abs : Abstraction)
  extends AbstractAction(s"Use $abs instead of ${(controller.graph, edge.target).shows}"){

  //TODO check keepOldUse and propagate redirection value
  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Redirection Action failure"){
        Redirection.redirectUsesAndPropagate(controller.graph.mileStone,
          edge, abs)

    }


}
