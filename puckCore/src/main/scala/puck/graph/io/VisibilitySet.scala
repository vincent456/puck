package puck.graph.io

import puck.graph.{DependencyGraph, NodeId}

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

  def apply() : T = Set[NodeId]()

  def allVisible(graph : DependencyGraph) : T = apply()

  def allHidden(graph : DependencyGraph) : T =
   apply().setVisibility(graph.nodesId.toSeq, Hidden)


  def violationsOnly(graph : DependencyGraph) : T =
    graph.violations().foldLeft(allHidden(graph)){
      (s, edge) =>
      val path1 = graph.containerPath(edge.source)
      val path2 = graph.containerPath(edge.target)

      s.setVisibility(path1, Visible)
        .setVisibility(path2, Visible)
    }

  implicit class VisibilitySetOps(val hiddens: T) extends AnyVal {

    def setVisibility(id : NodeId, v : Visibility) : T = v match {
      case Visible => hiddens - id
      case Hidden => hiddens + id
    }

    def setVisibility(ids : Seq[NodeId], v : Visibility) : T = v match {
      case Visible => hiddens -- ids
      case Hidden => hiddens ++ ids
    }

    def toggle(id : NodeId): T =
      setVisibility(id, visibility(id).opposite)


    def isVisible : NodeId => Boolean = id => !isHidden(id)
    def isHidden : NodeId => Boolean = hiddens.contains

    def visibility(id : NodeId) : Visibility =
      if(isHidden(id)) Hidden
      else Visible

  }

}

