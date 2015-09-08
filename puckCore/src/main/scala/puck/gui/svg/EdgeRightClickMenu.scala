package puck.gui.svg

import javax.swing.JPopupMenu

import puck.graph.{Isa, Uses, DGEdge}
import puck.gui.svg.actions.{ShowTypeRelationshipAction, RemoveEdgeAction, ManualSolveAction}

import NodeRightClickMenu.JPopupSyntax
class EdgeRightClickMenu
( private val controller : SVGController,
  edge : DGEdge)
  extends JPopupMenu {

  import controller.graph

  if(graph.isViolation(edge)){
    val target = graph.getConcreteNode(edge.target)
    add(new ManualSolveAction(target, controller))
  }

  if(edge.kind == Isa)
    add(new RemoveEdgeAction(edge, controller))

  edge match {
    case uses : Uses =>
      add(new ShowTypeRelationshipAction(Some(uses), controller))
      this.addMenuItem("Show type bindings (console)"){
        _ =>
          controller.printUseBindings(uses)
          graph.definitionOf(uses.user).foreach{
            userDef =>
              controller.printUseBindings(graph.getUsesEdge(userDef, uses.used).get)
          }
      }
    case _ => ()
  }

}
