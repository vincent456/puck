package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph._
import puck.gui.PushGraph

import scala.swing.Publisher

class AddIsaAction
(controller : Publisher,
 sub : ConcreteNode,
 sup : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends AbstractAction(s"Add ${sub.name} isa ${sup.name}") {

  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Make SuperType Action failure") {
      TR.makeSuperType(graph.mileStone, sub.id, sup.id)()
    }
}



class RemoveEdgeAction
(controller : Publisher,
 edge : DGEdge)
(implicit graph : DependencyGraph)
  extends AbstractAction(s"Delete $edge") {

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      edge.kind match {
        case Isa => LoggedSuccess(edge.deleteIn(graph.mileStone))
        case _ => LoggedError(s"cannot remove remove ${edge.kind} edge")
      }
    }
}

class RemoveNodeAction
(controller : Publisher,
 node : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends AbstractAction(s"Delete node and children") {

  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      TR.removeConcreteNode(graph.mileStone, node)
    }
}


class RenameNodeAction
(controller : Publisher,
 node : ConcreteNode )
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction("Rename") {

  import graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit = {
    showInputDialog("New name:", node.name).foreach {
      newName =>
          val g = TR.rename(graph.mileStone, node.id, newName)
          controller.publish(PushGraph(g))
    }
  }

}

class CreateInitalizerAction
(controller : Publisher,
 node : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Create initializer of $node") {

  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      LoggedSuccess(TR.intro.initializer(graph.mileStone, node.id)._2)
    }
}

class SetMutabilityAction
(controller : Publisher,
 node : ConcreteNode,
 mutable : Boolean)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Set $node " + (if(mutable) "mutable" else "immutable")) {

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Mutability Action failure"){
      LoggedSuccess(graph.setMutability(node.id, mutable))
    }
}

