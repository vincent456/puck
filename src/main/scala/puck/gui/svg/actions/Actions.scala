package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{DependencyGraph, ConcreteNode}
import puck.gui.svg.SVGController
import puck.javaGraph.transformations.JavaTransformationRules

import scalaz.{Success, Failure}

/**
 * Created by lorilan on 3/31/15.
 */
class AddIsaAction
( sub : ConcreteNode,
  sup : ConcreteNode,
  graph : DependencyGraph,
  controller : SVGController)
extends AbstractAction(s"Add ${sub.name} isa ${sup.name}") {

  def actionPerformed(e: ActionEvent) : Unit =
    controller.pushGraph(graph.addIsa(sub.id, sup.id))

}

class RemoveNodeAction
( node : ConcreteNode,
  graph : DependencyGraph,
  controller : SVGController)
extends AbstractAction(s"Delete node and children") {

  def actionPerformed(e: ActionEvent) : Unit = {
    JavaTransformationRules.removeConcreteNode(graph, node) match {
      case Failure(errs) =>
        errs.foreach(err => controller.console.appendText(err.getMessage))
      case Success(g) => controller.pushGraph(g)
    }
  }


}