package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JMenuItem, JPopupMenu}

import puck.graph.{Uses, Isa, DGEdge}
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

  edge match {
    case uses : Uses =>
      add(new ShowTypeRelationshipAction(Some(uses), controller))
    case _ => ()
  }

}
