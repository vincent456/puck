package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.transformations.rules.Redirection
import puck.gui.svg.SVGController
import puck.util.Logged

import scala.swing.Publisher

class RedirectAction0
( controller : Publisher,
  edge : Uses,
  abstractions : Seq[Abstraction])
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction("Use abstraction instead"){
  override def actionPerformed(e: ActionEvent): Unit =
    Choose("Redirect toward abtraction",
    s"Choose which abstraction to use instead of ${(graph, edge.target).shows}",
    abstractions,
      { lgAbs : Logged[Option[Abstraction]] =>
        val Some(abs) = lgAbs.value
        new RedirectAction(controller, edge, abs).actionPerformed(e)
      }
    )

}

class RedirectAction
(controller : Publisher,
 edge : Uses,
 abs : Abstraction)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Use $abs instead of ${(graph, edge.target).shows}"){

  //TODO check keepOldUse and propagate redirection value
  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Redirection Action failure"){
        Redirection.redirectUsesAndPropagate(graph.mileStone, edge, abs)
    }
}
