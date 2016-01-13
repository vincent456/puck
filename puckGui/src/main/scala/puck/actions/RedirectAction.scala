package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.transformations.rules.Redirection

import scala.swing.Publisher

class DisplayableAbstraction(val abs : Abstraction, graph : DependencyGraph) {

  override def toString : String = (graph, abs).shows

}

class RedirectAction0
( controller : Publisher,
  edge : Uses,
  abstractions : Seq[Abstraction])
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction("Use abstraction instead"){
  override def actionPerformed(e: ActionEvent): Unit = {
    println("Choose Abstraction !" + abstractions)

    val dAbstractions = abstractions map (new DisplayableAbstraction(_, graph))

    Choose( "Redirect toward abtraction",
      s"Choose which abstraction to use instead of ${(graph, edge.target).shows}",
      dAbstractions) match {
      case None =>()
      case Some(dAbs) =>
        printErrOrPushGraph(controller,"Redirection Action failure"){
          Redirection.redirectUsesAndPropagate(graph.mileStone, edge, dAbs.abs)
        }
    }
  }
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
