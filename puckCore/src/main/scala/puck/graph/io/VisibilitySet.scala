package puck.graph.io

import puck.graph.{NodeKind, DependencyGraph, NodeId}

trait Visibility{
  def opposite : Visibility
}
case object Hidden extends Visibility {
  def opposite = Visible
}
case object Visible extends Visibility {
  def opposite = Hidden
}

object VisibilitySet{

  type T = Set[NodeId]

  //def apply() : T =

  def allVisible(graph : DependencyGraph) : T = Set[NodeId]()


  def allHidden(graph : DependencyGraph) : T =
    Set[NodeId]().setVisibility(graph.nodesId, Visible)


  def violationsOnly(graph : DependencyGraph) : T =
    graph.violations().foldLeft(allHidden(graph)){
      (s, edge) =>
      val path1 = graph.containerPath(edge.source)
      val path2 = graph.containerPath(edge.target)

      s.setVisibility(path1, Visible)
        .setVisibility(path2, Visible)
    }
  
  def visibleKinds(graph : DependencyGraph, kinds : Set[NodeKind]) = {
    val visibleNodes = graph.nodes filter (kinds contains _.kind) map (_.id)
    allHidden(graph).setVisibility(visibleNodes, Visible)
  }


  def topLevelVisible(graph : DependencyGraph) =
    allHidden(graph).setVisibility(graph.content(DependencyGraph.rootId), Visible)

  
  implicit class VisibilitySetOps(val visibles: T) extends AnyVal {

    def setVisibility(id : NodeId, v : Visibility) : T = v match {
      case Visible => visibles - id
      case Hidden => visibles + id
    }

    def setVisibility(ids : Iterable[NodeId], v : Visibility) : T = v match {
      case Visible => visibles -- ids
      case Hidden => visibles ++ ids
    }

    def toggle(id : NodeId): T =
      setVisibility(id, visibility(id).opposite)


    def isVisible : NodeId => Boolean = id => !isHidden(id)
    def isHidden : NodeId => Boolean = visibles.contains

    def visibility(id : NodeId) : Visibility =
      if(isVisible(id)) Visible
      else Hidden

  }

}

