package puck.gui

import puck.graph._
import puck.graph.io._

import scala.swing.Publisher
import VisibilitySet.VisibilitySetOps
/**
  * Created by lorilan on 11/01/16.
  */
object PrintingOptionsControl {
  def apply(graph : DependencyGraph) : PrintingOptionsControl =
    new PrintingOptionsControl(VisibilitySet.topLevelVisible(graph).
      hideWithName(graph, Seq("@primitive")).
      hideWithName(graph, Seq("java")))


}

class PrintingOptionsControl
(private var _visibility : VisibilitySet.T
) extends Publisher {

  private var printId : Boolean = false
  private var printSignatures : Boolean = false
  private var printVirtualEdges : Boolean = true
  private var printConcreteUsesPerVirtualEdges : Boolean = true
  private var printRedOnly : Boolean = true
  private var selectedEdgeForTypePrinting0 : Option[Uses] = None


  def printingOptions =
    PrintingOptions(visibility, printId, printSignatures,
      selectedEdgeForTypePrinting0,
      printVirtualEdges, printConcreteUsesPerVirtualEdges,
      printRedOnly)

  def visibility : VisibilitySet.T = _visibility
  def visibility_=(v : VisibilitySet.T) : Unit = {
    _visibility = v
    this publish PrintingOptionsUpdate
  }

  def signatureVisible = printSignatures
  def signatureVisible_=(b : Boolean): Unit =
    if( b != printSignatures ){
      printSignatures = b
      this publish PrintingOptionsUpdate
    }


  def selectedEdgeForTypePrinting = selectedEdgeForTypePrinting0
  def selectedEdgeForTypePrinting_=(se: Option[Uses]) : Unit = {
    if( se != selectedEdgeForTypePrinting0 ){
      selectedEdgeForTypePrinting0 = se
      this publish PrintingOptionsUpdate
    }
  }


  def idVisible = printId
  def idVisible_=(b : Boolean): Unit = {
    if( b != printId ){
      printId = b
      this publish PrintingOptionsUpdate
    }
  }

  def virtualEdgesVisible = printVirtualEdges
  def virtualEdgesVisible_=(b : Boolean): Unit = {
    if( b != printVirtualEdges ){
      printVirtualEdges = b
      this publish PrintingOptionsUpdate
    }
  }
  def concreteUsesPerVirtualEdges = printConcreteUsesPerVirtualEdges
  def concreteUsesPerVirtualEdges_=(b : Boolean): Unit = {
    if( b != printConcreteUsesPerVirtualEdges ){
      printConcreteUsesPerVirtualEdges = b
      this publish PrintingOptionsUpdate
    }
  }

  def redEdgesOnly = printRedOnly
  def redEdgesOnly_=(b : Boolean): Unit = {
    if( b != printRedOnly ){
      printRedOnly = b
      this publish PrintingOptionsUpdate
    }
  }
  import VisibilitySet._
  def focusExpand(graph : DependencyGraph,
                  id : NodeId, focus : Boolean, expand : Boolean) : Unit = {
    if(focus)
      visibility = VisibilitySet.allHidden(graph).
        setVisibility(graph.containerPath(id), Visible)

    if(expand)
      visibility =
        graph.content(id).foldLeft(visibility)(_.setVisibility(_, Visible))

    this publish PrintingOptionsUpdate
  }

  def focus(graph : DependencyGraph,
            e : NodeIdP) : Unit = {
    val concretes = DGEdge.concreteEdgesFrom(graph, e)
    visibility = concretes.foldLeft(VisibilitySet.allHidden(graph)){
      case (set, DGEdge(_, source, target)) =>
        val s2 = set.setVisibility(graph.containerPath(source), Visible)
        s2.setVisibility(graph.containerPath(target), Visible)
    }
    this publish PrintingOptionsUpdate
  }

  private def setSubTreeVisibility(graph : DependencyGraph,
                                   rootId : NodeId, v : Visibility, includeRoot : Boolean): Unit ={
    val nodes = graph.subTree(rootId, includeRoot)
    visibility = visibility.setVisibility(nodes, v)
    this publish PrintingOptionsUpdate
  }
  def hide(graph : DependencyGraph, root : NodeId): Unit =
    setSubTreeVisibility(graph, root, Hidden, includeRoot = true)

  def collapse(graph : DependencyGraph, root: NodeId) : Unit =
    setSubTreeVisibility(graph, root, Hidden, includeRoot = false)

  def expandAll(graph : DependencyGraph, root: NodeId) : Unit =
    setSubTreeVisibility(graph, root, Visible, includeRoot = true)
}
