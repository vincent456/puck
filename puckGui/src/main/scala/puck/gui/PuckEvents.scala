package puck.gui

import puck.graph._
import puck.graph.io.{Svg, DotOutputFormat, VisibilitySet}
import puck.search.Search

import scala.swing.event.Event

case class NodeClicked(node : DGNode) extends Event

case class GraphUpdate(graph : DependencyGraph) extends Event

case object LoadCodeRequest extends Event
case object LoadConstraintRequest extends Event
case class GraphDisplayRequest
(title : String,
 graph : DependencyGraph,
 printId : Boolean,
 printSignature : Boolean,
 visibility : VisibilitySet.T,
 sUse : Option[Uses] = None,
 format : DotOutputFormat = Svg)
  extends Event

case class ConstraintDisplayRequest(graph : DependencyGraph) extends Event
case class GraphExplorerFocus(e : DGEdge) extends Event

case class ApplyOnCodeRequest(searchResult : DependencyGraph) extends Event
