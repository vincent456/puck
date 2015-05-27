package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph.{DGUses, DGEdge, ConcreteNode}
import puck.gui.svg.SVGController

import scalaz._

class AddIsaAction
( sub : ConcreteNode,
  sup : ConcreteNode,
  controller : SVGController)
extends AbstractAction(s"Add ${sub.name} isa ${sup.name}") {

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Make SuperType Action failure") {
      TR.makeSuperType(graph.mileStone, sub.id, sup.id)
    }

}

class ShowTypeRelationshipAction
( edge : Option[DGUses],
  controller : SVGController)
  extends AbstractAction(s"Show type relationship")
{
  def actionPerformed(e: ActionEvent) : Unit =
    controller.setSelectedEdgeForTypePrinting(edge)
}

class RemoveEdgeAction
( edge : DGEdge,
  controller : SVGController)
  extends AbstractAction(s"Delete node and children") {

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      edge.kind match {
        case DGEdge.IsaK => \/-(edge.deleteIn(controller.graph.mileStone))
        case _ => -\/(new PuckError(s"cannot remove remove ${edge.kind} edge"))
      }


    }
}

class RemoveNodeAction
( node : ConcreteNode,
  controller : SVGController)
extends AbstractAction(s"Delete node and children") {

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      TR.removeConcreteNode(graph.mileStone, node)
    }
}



class RenameNodeAction
( node : ConcreteNode,
  controller : SVGController )
  extends AbstractAction("Rename") {

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit = {
    showInputDialog("New name:").foreach {
      newName =>
          val g = TR.rename(graph.mileStone, node.id, newName)
          controller.pushGraph(g)
    }
  }

}