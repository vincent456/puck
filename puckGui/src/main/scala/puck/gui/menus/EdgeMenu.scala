package puck.gui.menus

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JPopupMenu}

import puck.actions.{RedirectAction0, RemoveEdgeAction}
import puck.graph._
import puck.gui.svg.actions._
import puck.gui._

import scala.swing.Publisher


class EdgeMenu
( publisher : Publisher,
  edge : NodeIdP,
  printingOptionsControl: PrintingOptionsControl,
  blurrySelection : Boolean,
  implicit val graph: DependencyGraph,
  implicit val graphUtils: GraphUtils)
  extends JPopupMenu {


  val (source, target) = edge

  println("target" + graph.getConcreteNode(target))
  println("abstractions " +graph.abstractions(target))

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

  def addUsesActions(src : NodeId, tgt : NodeId) : Unit =
    graph.getUsesEdge(src, tgt) foreach {
      uses =>
        isUseEdge = true
        add(new AbstractAction(s"Show type bindings"){
          def actionPerformed(e: ActionEvent) : Unit =
            publisher publish EdgeForTypePrinting(Some(uses))
        })


        val abstractions = graph.abstractions(tgt)
        if(abstractions.nonEmpty)
          add(new RedirectAction0(publisher, uses, abstractions.toSeq))
    }



  if(blurrySelection)
    graph.nodePlusDefAndParams(source).foreach{
      userDef => addUsesActions(userDef, target)
    }
  else
    addUsesActions(source, target)


  val isConcreteEdge = isIsaEdge || isUseEdge


  if(!isConcreteEdge)
      this.addMenuItem("Focus"){
        _ =>
          publisher publish GraphFocus(graph, AbstractEdgeKind(edge))
      }



}
