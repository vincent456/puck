package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JMenuItem, JPopupMenu}

import puck.graph.{Isa, DGEdge}
import puck.gui.svg.actions.{ShowTypeRelationshipAction, RemoveEdgeAction, SolveAction}

class EdgeRightClickMenu
( private val controller : SVGController,
  edge : DGEdge)
  extends JPopupMenu {

  import controller.graph

  if(graph.isViolation(edge)){
    val target = graph.getConcreteNode(edge.target)
    add(new SolveAction(target, controller))
  }

  if(edge.kind == DGEdge.IsaK)
    add(new RemoveEdgeAction(edge, controller))

  add(new ShowTypeRelationshipAction(Some(edge), controller))
}
