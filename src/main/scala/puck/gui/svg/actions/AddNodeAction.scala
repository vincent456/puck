package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JOptionPane}

import puck.graph.{ConcreteNode, NodeKind}
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.{Class, Interface, Package}

class AddNodeAction
( host : ConcreteNode,
  controller : SVGController,
  childKind : NodeKind)
extends AbstractAction(s"Add $childKind")
{

  import controller._
  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    childKind match {
      case Package
           | Interface
           | Class =>
        showInputDialog(s"New $childKind name:").foreach {
          childName =>
            val (n, g) = transfoRules.intro(graph.mileStone, childName, childKind, None)
            pushGraph(g.addContains(host.id, n.id))
        }
      case _ =>
        JOptionPane.showMessageDialog(null, s"add of $childKind not implemented",
          "Error", JOptionPane.ERROR_MESSAGE);
    }

  }
}
