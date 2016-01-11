package puck.gui.svg

import java.awt.Component
import javax.swing.JPopupMenu

import puck.actions.RemoveEdgeAction
import puck.graph.io.PrintingOptions
import puck.gui.svg.actions.ShowTypeRelationshipGraphicAction
import puck.graph._
import puck.gui.svg.actions._


class SVGEdgeMenu
( publisher : SVGController,
  edge : NodeIdP)
  extends JPopupMenu {

  implicit val graph: DependencyGraph = publisher.graphStack.graph
  implicit val graphUtils: GraphUtils = publisher.graphUtils

  val (source, target) = edge

  if(graph.isViolation(edge)){
    val targetNode = graph.getConcreteNode(target)
    add(new ManualSolveAction(publisher, targetNode))
    add(new AutoSolveAction(publisher, targetNode,
      publisher.printingOptionsControl.printingOptions))
  }

  var isIsaEdge = false
  var isUseEdge = false

  if(graph.isa(source, target)) {
    isIsaEdge = true
    add(new RemoveEdgeAction(publisher, Isa(source, target)))
  }

  def addShowBRActions(src : NodeId, tgt : NodeId) : Unit =
    graph.getUsesEdge(src, tgt) foreach {
      uses =>
        isUseEdge = true
        add(new ShowTypeRelationshipGraphicAction(Some(uses), publisher))
        add(new ShowTypeRelationshipTextualAction(Some(uses), publisher))
    }

  addShowBRActions(source, target)

  graph.definitionOf(source).foreach{
    userDef => addShowBRActions(userDef, target)
  }

  val isConcreteEdge = isIsaEdge || isUseEdge

  if(!isConcreteEdge)
      this.addMenuItem("Focus"){
        _ =>
          publisher.printingOptionsControl.focus(edge)
      }

}
