package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.transformations.rules.Redirection
import puck.gui.svg.SVGController

import scala.swing.Publisher

case class RedirectAction
(controller : Publisher,
 newTarget : ConcreteNode,
 edge : Uses,
 abs : Abstraction)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Use $abs instead of ${(graph, edge.target).shows}"){

  //TODO check keepOldUse and propagate redirection value
  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Redirection Action failure"){
        Redirection.redirectUsesAndPropagate(graph.mileStone,
          edge, abs)

    }


}
