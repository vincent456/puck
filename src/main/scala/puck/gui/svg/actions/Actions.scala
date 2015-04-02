package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.ConcreteNode
import puck.gui.svg.SVGController
import puck.javaGraph.transformations.JavaTransformationRules

import scalaz.{Success, Failure}

/**
 * Created by lorilan on 3/31/15.
 */
class AddIsaAction
( sub : ConcreteNode,
  sup : ConcreteNode,
  controller : SVGController)
extends AbstractAction(s"Add ${sub.name} isa ${sup.name}") {

  import controller.graph

  def actionPerformed(e: ActionEvent) : Unit =
    controller.pushGraph(graph.addIsa(sub.id, sup.id))

}

class RemoveNodeAction
( node : ConcreteNode,
  controller : SVGController)
extends AbstractAction(s"Delete node and children") {

  import controller.graph

  def actionPerformed(e: ActionEvent) : Unit = {
    JavaTransformationRules.removeConcreteNode(graph, node) match {
      case Failure(errs) =>
        errs.foreach(err => controller.console.appendText(err.getMessage))
      case Success(g) => controller.pushGraph(g)
    }
  }
}

class RenameNodeAction
( node : ConcreteNode,
  controller : SVGController )
  extends AbstractAction("Rename") {

  import controller.graph
  override def actionPerformed(e: ActionEvent): Unit = {
    showInputDialog("New name:").foreach {
      newName =>
          controller.pushGraph(graph.setName(node.id, newName))
        /*graph.kindType(node.id) match {
          case TypeMember =>
          case _ =>
        }*/

    }
  }

}