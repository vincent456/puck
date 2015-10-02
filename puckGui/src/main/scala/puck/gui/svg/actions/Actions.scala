package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph._
import puck.gui.svg.SVGController

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

class ShowTypeRelationshipGraphicAction
( edge : Option[DGUses],
  controller : SVGController)
  extends AbstractAction(s"Show type bindings (graphic)")
{
  def actionPerformed(e: ActionEvent) : Unit =
    controller.setSelectedEdgeForTypePrinting(edge)
}

class ShowTypeRelationshipTextualAction
( edge : Option[DGUses],
  controller : SVGController)
  extends AbstractAction(s"Show type bindings (text)")
{
  import controller.graph
  def actionPerformed(e: ActionEvent) : Unit =
    edge.foreach {
      uses =>
      controller.printUseBindings(uses)
      graph.definitionOf(uses.user).foreach{
        userDef =>
          controller.printUseBindings(graph.getUsesEdge(userDef, uses.used).get)
      }
  }

}

class RemoveEdgeAction
( edge : DGEdge,
  controller : SVGController)
  extends AbstractAction(s"Delete $edge") {

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      edge.kind match {
        case Isa => LoggedSuccess(edge.deleteIn(controller.graph.mileStone))
        case _ => LoggedError(s"cannot remove remove ${edge.kind} edge")
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

class CreateInitalizerAction
( node : ConcreteNode,
  controller : SVGController)
  extends AbstractAction(s"Create initializer of $node") {

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      LoggedSuccess(TR.intro.initializer(graph.mileStone, node.id)._2)
    }
}