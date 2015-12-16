package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{ConcreteNode, NodeKind}

class AddNodeAction
(controller : GraphController,
 host : ConcreteNode,
 childKind : NodeKind)
extends AbstractAction(s"Add $childKind")
{

  import controller._
  import graphUtils.{transformationRules => TR}

  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    showInputDialog(s"New $childKind name:").foreach {
      childName =>
        val (n, g) = TR.intro(graph.mileStone, childName, childKind)
        pushGraph(g.addContains(host.id, n.id))
    }
  }
}
