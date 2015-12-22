package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph._

class AddIsaAction
(controller : GraphController,
 sub : ConcreteNode,
 sup : ConcreteNode)
extends AbstractAction(s"Add ${sub.name} isa ${sup.name}") {

  import controller.{graph, graphUtils}
  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Make SuperType Action failure") {
      TR.makeSuperType(graph.mileStone, sub.id, sup.id)()
    }
}



class RemoveEdgeAction
(controller : GraphController,
 edge : DGEdge)
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
(controller : GraphController,
 node : ConcreteNode)
extends AbstractAction(s"Delete node and children") {

  import controller.{graph, graphUtils}
  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      TR.removeConcreteNode(graph.mileStone, node)
    }
}


class RenameNodeAction
(controller : GraphController,
 node : ConcreteNode )
  extends AbstractAction("Rename") {

  import controller.{graph, graphUtils}
  import graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit = {
    showInputDialog("New name:").foreach {
      newName =>
          val g = TR.rename(graph.mileStone, node.id, newName)
          controller.pushGraph(g)
    }
  }

}

class CreateInitalizerAction
(controller : GraphController,
 node : ConcreteNode)
  extends AbstractAction(s"Create initializer of $node") {

  import controller.{graph, graphUtils}
  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      LoggedSuccess(TR.intro.initializer(graph.mileStone, node.id)._2)
    }
}

class SetMutabilityAction
(controller : GraphController,
 node : ConcreteNode,
 mutable : Boolean)
  extends AbstractAction(s"Set $node " + (if(mutable) "mutable" else "immutable")) {

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Mutability Action failure"){
      LoggedSuccess( controller.graph.setMutability(node.id, mutable))
    }
}

