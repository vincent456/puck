package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph._

class AddIsaAction
( controller : UtilGraphStack,
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
( controller : UtilGraphStack,
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
( controller : UtilGraphStack,
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
( controller : UtilGraphStack,
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
( controller : UtilGraphStack,
  node : ConcreteNode)
  extends AbstractAction(s"Create initializer of $node") {

  import controller.{graph, graphUtils}
  import graphUtils.{transformationRules => TR}

  def actionPerformed(e: ActionEvent) : Unit =
    printErrOrPushGraph(controller, "Remove Node Action failure"){
      LoggedSuccess(TR.intro.initializer(graph.mileStone, node.id)._2)
    }
}