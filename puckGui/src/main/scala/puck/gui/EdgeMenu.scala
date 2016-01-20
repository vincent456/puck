package puck.gui

import javax.swing.JPopupMenu

import puck.actions.{RedirectAction0, Choose, RemoveEdgeAction}
import puck.graph._
import puck.graph.io.PrintingOptions
import puck.gui.svg.actions.{ShowTypeRelationshipAction, _}

import scala.swing.Publisher


class EdgeMenu
( publisher : Publisher,
  edge : NodeIdP,
  printingOptionsControl: PrintingOptionsControl,
  implicit val graph: DependencyGraph,
  implicit val graphUtils: GraphUtils)
  extends JPopupMenu {



  val (source, target) = edge

  if(graph.isViolation(edge)){
    val targetNode = graph.getConcreteNode(target)
    //add(new ManualSolveAction(publisher, targetNode))
    add(new AutoSolveAction(publisher, targetNode, printingOptionsControl))
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
        add(new ShowTypeRelationshipAction(Some(uses), publisher))

        val abstractions = graph.abstractions(target)
        if(abstractions.nonEmpty)
          add(new RedirectAction0(publisher, uses, abstractions.toSeq))




    }

  addShowBRActions(source, target)

  graph.definitionOf(source).foreach{
    userDef => addShowBRActions(userDef, target)
  }

  val isConcreteEdge = isIsaEdge || isUseEdge


  if(!isConcreteEdge)
      this.addMenuItem("Focus"){
        _ =>
          publisher publish GraphFocus(graph, AbstractEdgeKind(edge))
      }



}
