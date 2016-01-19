package puck.gui

import javax.swing.JPopupMenu

import puck.graph.{NodeId, GraphUtils, DependencyGraph}
import puck.gui.svg.actions.{AutoSolveAction, ManualSolveAction}

import scala.swing.Publisher

/**
  * Created by lorilan on 17/12/15.
  */
class ViolationMenu
(publisher : Publisher,
 target : NodeId,
 printingOptionsControl: PrintingOptionsControl)
(implicit graph : DependencyGraph,
  graphUtils : GraphUtils)
  extends JPopupMenu {

  val targetNode = graph.getConcreteNode(target)
  add(new ManualSolveAction(publisher, targetNode))
  add(new AutoSolveAction(publisher, targetNode, printingOptionsControl))

}
