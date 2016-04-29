/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.gui

import puck.graph._
import puck.graph.io._

import scala.swing.Publisher
import VisibilitySet.VisibilitySetOps
/**
  * Created by Loïc Girault on 11/01/16.
  */
object PrintingOptionsControl {
  def apply(graph : DependencyGraph,
            bus : Publisher) : PrintingOptionsControl =
    new PrintingOptionsControl(
      VisibilitySet.topLevelVisible(graph).
      hideWithName(graph, Seq("@primitive")).
      hideWithName(graph, Seq("java")), bus)


}

class PrintingOptionsControl
(private var _visibility : VisibilitySet.T,
 bus : Publisher) {

  private var printId : Boolean = false
  private var printSignatures : Boolean = false
  private var printVirtualEdges : Boolean = true
  private var printConcreteUsesPerVirtualEdges : Boolean = true
  private var printRedOnly : Boolean = true
  private var selectedEdgeForTypePrinting0 : Option[NodeIdP] = None
  private var printTypeUses : Boolean = false

  def printingOptions =
    PrintingOptions(visibility, printId, printSignatures,
      selectedEdgeForTypePrinting0,
      printVirtualEdges, printTypeUses,
      printConcreteUsesPerVirtualEdges,
      printRedOnly)

  def visibility : VisibilitySet.T = _visibility
  def visibility_=(v : VisibilitySet.T) : Unit = {
    _visibility = v
    bus publish PrintingOptionsUpdate
  }

  def signatureVisible = printSignatures
  def signatureVisible_=(b : Boolean): Unit =
    if( b != printSignatures ){
      printSignatures = b
      bus publish PrintingOptionsUpdate
    }


  def selectedEdgeForTypePrinting = selectedEdgeForTypePrinting0
  def selectedEdgeForTypePrinting_=(se: Option[NodeIdP]) : Unit = {
    if( se != selectedEdgeForTypePrinting0 ){
      selectedEdgeForTypePrinting0 = se
      bus publish PrintingOptionsUpdate
    }
  }


  def idVisible = printId
  def idVisible_=(b : Boolean): Unit = {
    if( b != printId ){
      printId = b
      bus publish PrintingOptionsUpdate
    }
  }

  def virtualEdgesVisible = printVirtualEdges
  def virtualEdgesVisible_=(b : Boolean): Unit = {
    if( b != printVirtualEdges ){
      printVirtualEdges = b
      bus publish PrintingOptionsUpdate
    }
  }
  def concreteUsesPerVirtualEdges = printConcreteUsesPerVirtualEdges
  def concreteUsesPerVirtualEdges_=(b : Boolean): Unit = {
    if( b != printConcreteUsesPerVirtualEdges ){
      printConcreteUsesPerVirtualEdges = b
      bus publish PrintingOptionsUpdate
    }
  }

  def typeUsesVisible = printTypeUses
  def typeUsesVisible_=(b : Boolean): Unit = {
    if( b != printTypeUses ){
      printTypeUses = b
      bus publish PrintingOptionsUpdate
    }
  }

  def redEdgesOnly = printRedOnly
  def redEdgesOnly_=(b : Boolean): Unit = {
    if( b != printRedOnly ){
      printRedOnly = b
      bus publish PrintingOptionsUpdate
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

    bus publish PrintingOptionsUpdate
  }

  def focus(graph : DependencyGraph,
            e : NodeIdP) : Unit = {
    val concretes = DGEdge.concreteEdgesFrom(graph, e)
    visibility = concretes.foldLeft(VisibilitySet.allHidden(graph)){
      case (set, DGEdge(_, source, target)) =>
        val s2 = set.setVisibility(graph.containerPath(source), Visible)
        s2.setVisibility(graph.containerPath(target), Visible)
    }
    bus publish PrintingOptionsUpdate
  }

  private def setSubTreeVisibility(graph : DependencyGraph,
                                   rootId : NodeId, v : Visibility, includeRoot : Boolean): Unit ={
    val nodes = graph.subTree(rootId, includeRoot)
    visibility = visibility.setVisibility(nodes, v)
    bus publish PrintingOptionsUpdate
  }
  def hide(graph : DependencyGraph, root : NodeId): Unit =
    setSubTreeVisibility(graph, root, Hidden, includeRoot = true)

  def collapse(graph : DependencyGraph, root: NodeId) : Unit =
    setSubTreeVisibility(graph, root, Hidden, includeRoot = false)

  def expandAll(graph : DependencyGraph, root: NodeId) : Unit =
    setSubTreeVisibility(graph, root, Visible, includeRoot = true)
}
