package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JMenuItem, JPopupMenu}

import puck.graph.{Isa, DGEdge}
import puck.gui.svg.actions.{RemoveEdgeAction, SolveAction}

/**
 * Created by lorilan on 3/31/15.
 */
class EdgeRightClickMenu
( private val controller : SVGController,
  edge : DGEdge)
  extends JPopupMenu {

  import controller.graph

  if(graph.isViolation(edge)){
    val target = graph.getConcreteNode(edge.target)
    add(new SolveAction(target, controller))
  }

  if(edge.kind == Isa)
    add(new RemoveEdgeAction(edge, controller))

}
