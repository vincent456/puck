package puck.gui

import puck.graph._
import puck.graph.io.{Svg, DotOutputFormat, VisibilitySet}
import puck.search.Search

import scala.swing.event.Event

case class PuckTreeNodeClicked(graph : DependencyGraph, node : NodeId) extends Event

trait GraphEvent extends Event {
  val graph : DependencyGraph
}

case class DGUpdate(graph : DependencyGraph) extends GraphEvent
case class CCUpdate(graph : DependencyGraph) extends GraphEvent
case class SetVisibleFromKind(ks : Seq[NodeKind]) extends Event
case object SetTopLevelVisible extends Event


sealed trait ControlRequest extends Event

case object LoadCodeRequest extends ControlRequest
case object LoadConstraintRequest extends ControlRequest
case class GraphDisplayRequest
(title : String,
 graph : DependencyGraph,
 printId : Boolean,
 printSignature : Boolean,
 visibility : VisibilitySet.T,
 sUse : Option[Uses] = None,
 format : DotOutputFormat = Svg)
  extends ControlRequest

case class ConstraintDisplayRequest(graph : DependencyGraph) extends ControlRequest
//case class ExploreRequest
//(builder : ConstraintSolvingSearchEngineBuilder)
//  extends ControlRequest

//case class SearchStateMapPrintingRequest
//(stateMap : Map[Int, Seq[SearchState[SResult]]],
// printId : Boolean,
// printSignature : Boolean,
// visibility : VisibilitySet.T)
//  extends ControlRequest

//case class SearchStateSeqPrintingRequest
//(subDir : String,
// states : Seq[SearchState[SResult]],
// sPrinter : Option[SearchState[SResult] => String],
// printId : Boolean,
// printSignature : Boolean,
// visibility : VisibilitySet.T)
//  extends ControlRequest

case class ApplyOnCodeRequest(searchResult : DependencyGraph) extends ControlRequest

sealed abstract class Answer extends Event
case class ExplorationFinished(result : Search[SResult]) extends Answer