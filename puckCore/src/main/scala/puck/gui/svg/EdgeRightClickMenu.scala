package puck.gui.svg

import javax.swing.JPopupMenu

import puck.graph.{Uses, DGEdge}
import puck.gui.svg.actions.{ShowTypeRelationshipAction, RemoveEdgeAction, ManualSolveAction}

class EdgeRightClickMenu
( private val controller : SVGController,
  edge : DGEdge)
  extends JPopupMenu {

  import controller.graph

  if(graph.isViolation(edge)){
    val target = graph.getConcreteNode(edge.target)
    add(new ManualSolveAction(target, controller))
  }

  if(edge.kind == DGEdge.IsaK)
    add(new RemoveEdgeAction(edge, controller))

  edge match {
    case uses : Uses =>
      add(new ShowTypeRelationshipAction(Some(uses), controller))
    case _ => ()
  }

}
