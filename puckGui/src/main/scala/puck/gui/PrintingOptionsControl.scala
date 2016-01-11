package puck.gui

import puck.GraphStack
import puck.graph.{DGEdge, NodeIdP, NodeId, Uses}
import puck.graph.io._

import scala.swing.Publisher

/**
  * Created by lorilan on 11/01/16.
  */
class PrintingOptionsControl
(val graphStack: GraphStack) extends Publisher {

  private var visibility : VisibilitySet.T = VisibilitySet.allVisible(graphStack.graph)
  private var printId : Boolean = false
  private var printSignatures : Boolean = false
  private var printVirtualEdges : Boolean = true
  private var printConcreteUsesPerVirtualEdges : Boolean = true
  private var printRedOnly : Boolean = true
  private var selectedEdgeForTypePrinting : Option[Uses] = None


  import graphStack.graph

  def printingOptions =
    PrintingOptions(visibility, printId, printSignatures,
      selectedEdgeForTypePrinting,
      printVirtualEdges, printConcreteUsesPerVirtualEdges,
      printRedOnly)

  def setSignatureVisible(b : Boolean): Unit = {
    if( b != printSignatures ){
      printSignatures = b
      this publish GraphUpdate(graph)
    }
  }
  def setSelectedEdgeForTypePrinting(se: Option[Uses]) : Unit = {
    if( se != selectedEdgeForTypePrinting ){
      selectedEdgeForTypePrinting = se
      this publish GraphUpdate(graph)
    }
  }

  def setIdVisible(b : Boolean): Unit = {
    if( b != printId ){
      printId = b
      this publish GraphUpdate(graph)
    }
  }

  def setVirtualEdgesVisible(b : Boolean): Unit = {
    if( b != printVirtualEdges ){
      printVirtualEdges = b
      this publish GraphUpdate(graph)
    }
  }
  def setConcreteUsesPerVirtualEdges(b : Boolean): Unit = {
    if( b != printConcreteUsesPerVirtualEdges ){
      printConcreteUsesPerVirtualEdges = b
      this publish GraphUpdate(graph)
    }
  }

  def setRedEdgesOnly(b : Boolean): Unit = {
    if( b != printRedOnly ){
      printRedOnly = b
      this publish GraphUpdate(graph)
    }
  }
  import VisibilitySet._
  def focusExpand(id : NodeId, focus : Boolean, expand : Boolean) : Unit = {
    if(focus)
      visibility = VisibilitySet.allHidden(graph).
        setVisibility(graph.containerPath(id), Visible)

    if(expand)
      visibility =
        graph.content(id).foldLeft(visibility)(_.setVisibility(_, Visible))

    this publish GraphUpdate(graph)
  }

  def focus(e : NodeIdP) : Unit = {
    val concretes = DGEdge.concreteEdgesFrom(graph, e)
    visibility = concretes.foldLeft(VisibilitySet.allHidden(graph)){
      case (set, DGEdge(_, source, target)) =>
        val s2 = set.setVisibility(graph.containerPath(source), Visible)
        s2.setVisibility(graph.containerPath(target), Visible)
    }
    this publish GraphUpdate(graph)
  }

  private def setSubTreeVisibility(rootId : NodeId, v : Visibility, includeRoot : Boolean): Unit ={
    val nodes = graph.subTree(rootId, includeRoot)
    visibility = visibility.setVisibility(nodes, v)
    this publish GraphUpdate(graph)
  }
  def hide(root : NodeId): Unit =
    setSubTreeVisibility(root, Hidden, includeRoot = true)

  def collapse(root: NodeId) : Unit =
    setSubTreeVisibility(root, Hidden, includeRoot = false)

  def expandAll(root: NodeId) : Unit =
    setSubTreeVisibility(root, Visible, includeRoot = true)
}
