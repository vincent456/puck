package puck.gui

import javax.swing.JPopupMenu

import puck.graph.NodeIdP
import puck.graph.io.PrintingOptions
import puck.gui.svg.actions.{SwingGraphController, AutoSolveAction, ManualSolveAction}

/**
  * Created by lorilan on 17/12/15.
  */
class ViolationMenu
(private val controller : SwingGraphController,
 edge : NodeIdP)
  extends JPopupMenu {

  val (source, target) = edge

  import controller.graph

  val targetNode = graph.getConcreteNode(target)
  add(new ManualSolveAction(targetNode, controller))
  add(new AutoSolveAction(graph getConcreteNode target, controller,
    PrintingOptions(Set())))

}
