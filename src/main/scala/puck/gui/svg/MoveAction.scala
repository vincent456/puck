package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{DependencyGraph, DGNode}

import scalaz.{Success, Failure}

/**
 * Created by lorilan on 3/18/15.
 */
class MoveAction
( host : DGNode,
  moved : DGNode,
  graph : DependencyGraph,
  controller : SVGController)
extends AbstractAction(s"Move $moved here"){

  override def actionPerformed(e: ActionEvent): Unit = {
    controller.transfoRules.moveTo(graph, moved.id, host.id) match {
      case Failure(errs) =>
        controller.console.appendText("Abstraction creation failure\n" )
        errs.foreach(e => controller.console.appendText(e.getMessage + "\n"))
      case Success(g) => controller.pushGraph(g)
    }
  }
}
