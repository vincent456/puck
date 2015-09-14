package puck.gui.svg

import javax.swing.JPopupMenu

import puck.graph.{Isa, Uses, NodeIdP}
import puck.gui.svg.actions.{ShowTypeRelationshipAction, RemoveEdgeAction, ManualSolveAction}

import NodeRightClickMenu.JPopupSyntax
class EdgeRightClickMenu
( private val controller : SVGController,
  edge : NodeIdP)
  extends JPopupMenu {

  val (source, target) = edge
  import controller.graph

  if(graph.isViolation(edge)){
    val targetNode = graph.getConcreteNode(target)
    add(new ManualSolveAction(targetNode, controller))
  }

  var isIsaEdge = false
  var isUseEdge = false

  if(graph.isa(source, target)) {
    isIsaEdge = true
    add(new RemoveEdgeAction(Isa(source, target), controller))
  }

  graph.getUsesEdge(source, target) match {
    case Some(uses)=>
      isUseEdge = true
      add(new ShowTypeRelationshipAction(Some(uses), controller))
      this.addMenuItem("Show type bindings (console)"){
        _ =>
          controller.printUseBindings(uses)
          graph.definitionOf(uses.user).foreach{
            userDef =>
              controller.printUseBindings(graph.getUsesEdge(userDef, uses.used).get)
          }
      }
    case None => ()
  }

  val isConcreteEdge = isIsaEdge || isUseEdge

  if(!isConcreteEdge)
      this.addMenuItem("Focus"){
        _ =>
        controller.focus(edge)
      }

}
