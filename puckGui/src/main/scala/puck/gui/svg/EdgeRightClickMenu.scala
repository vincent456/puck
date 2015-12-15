package puck.gui.svg

import javax.swing.JPopupMenu

import puck.actions.RemoveEdgeAction
import puck.gui.svg.actions.ShowTypeRelationshipGraphicAction
import puck.graph.{NodeId, Isa, Uses, NodeIdP}
import puck.gui.svg.actions._

class EdgeRightClickMenu
( private val controller : SVGController,
  edge : NodeIdP)
  extends JPopupMenu {

  val (source, target) = edge
  import controller.graph

  if(graph.isViolation(edge)){
    val targetNode = graph.getConcreteNode(target)
    add(new ManualSolveAction(targetNode, controller))
    add(new AutoSolveAction(targetNode, controller))
  }

  var isIsaEdge = false
  var isUseEdge = false

  if(graph.isa(source, target)) {
    isIsaEdge = true
    add(new RemoveEdgeAction(controller, Isa(source, target)))
  }

  def addShowBRActions(src : NodeId, tgt : NodeId) : Unit =
    graph.getUsesEdge(src, tgt) foreach {
      uses =>
        isUseEdge = true
        add(new ShowTypeRelationshipGraphicAction(Some(uses), controller))
        add(new ShowTypeRelationshipTextualAction(Some(uses), controller))
    }

  addShowBRActions(source, target)

  graph.definitionOf(source).foreach{
    userDef => addShowBRActions(userDef, target)
  }

  val isConcreteEdge = isIsaEdge || isUseEdge

  if(!isConcreteEdge)
      this.addMenuItem("Focus"){
        _ =>
        controller.focus(edge)
      }

}
