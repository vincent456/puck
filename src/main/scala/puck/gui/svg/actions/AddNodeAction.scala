package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JOptionPane}

import puck.graph.{ConcreteNode, NodeKind}
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.{Class, Interface, Package}

/**
 * Created by lorilan on 29/03/15.
 */
class AddNodeAction
( host : ConcreteNode,
  controller : SVGController,
  childKind : NodeKind)
extends AbstractAction(s"Add $childKind")
{
  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    childKind match {
      case Package
           | Interface
           | Class =>
        val childName = JOptionPane.showInputDialog(s"New $childKind name:")
        val (n, g) =  controller.graph.addConcreteNode(childName, childKind, None)
        controller.pushGraph(g.addContains(host.id, n.id))
      case _ =>
        JOptionPane.showMessageDialog(null, s"add of $childKind not implemented",
          "Error", JOptionPane.ERROR_MESSAGE);
    }

  }
}
