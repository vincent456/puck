package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.{JOptionPane, AbstractAction}

import puck.graph.{NodeKind, ConcreteNode}
import puck.javaGraph.nodeKind.{Package, Interface, Class}

/**
 * Created by lorilan on 29/03/15.
 */
case class AddNodeAction
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
        val (n, g) =  controller.getGraph.addConcreteNode(childName, childKind, None)
        controller.pushGraph(g.addContains(host.id, n.id))
      case _ =>
        JOptionPane.showMessageDialog(null, s"add of $childKind not implemented",
          "Error", JOptionPane.ERROR_MESSAGE);
    }

  }
}
