package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{GraphUtils, DependencyGraph, ConcreteNode, NodeKind}
import puck.gui.PushGraph

import scala.swing.Publisher

class AddNodeAction
(publisher : Publisher,
 host : ConcreteNode,
 childKind : NodeKind)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends AbstractAction(s"Add $childKind")
{

  import graphUtils.{transformationRules => TR}

  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    showInputDialog(s"New $childKind name:").foreach {
      childName =>
        val (n, g) = TR.intro(graph.mileStone, childName, childKind)
        publisher.publish(PushGraph(g.addContains(host.id, n.id)))
    }
  }
}
