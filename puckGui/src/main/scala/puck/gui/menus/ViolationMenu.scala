package puck.gui.menus

import javax.swing.JPopupMenu

import puck.graph.{DependencyGraph, GraphUtils, NodeId}
import puck.gui.PrintingOptionsControl
import puck.gui.svg.actions.AutoSolveAction

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
  //add(new ManualSolveAction(publisher, targetNode))
  add(new AutoSolveAction(publisher, targetNode, printingOptionsControl))

}
