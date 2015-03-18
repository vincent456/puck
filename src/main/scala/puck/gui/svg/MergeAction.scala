package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{DependencyGraph, ConcreteNode}

import scalaz.{Success, Failure}

/**
 * Created by lorilan on 3/18/15.
 */
case class MergeAction(
  consumed : ConcreteNode,
  consumer : ConcreteNode,
  graph : DependencyGraph,
  controller : SVGController)
  extends AbstractAction(s"Merge $consumed into this") {

  override def actionPerformed(e: ActionEvent): Unit = {
    controller.transfoRules.merge(graph, consumer.id, consumed.id) match {
      case Failure(errs) =>
        controller.console.appendText("Abstraction creation failure\n" )
        errs.foreach(e => controller.console.appendText(e.getMessage + "\n"))
      case Success(g) => controller.pushGraph(g)
    }
  }


}
