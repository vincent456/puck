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

/**
 * Created by lorilan on 17/11/14.
 */
object VisibilitySet{
  def apply() = new VisibilitySet(Set[NodeId]())

  def allVisible(graph : DependencyGraph) = apply()

  def allHidden(graph : DependencyGraph) ={
    val s = apply()
    s.setVisibility(graph.nodesId.toSeq, Hidden)
    s
  }

  def violationsOnly(graph : DependencyGraph) : VisibilitySet = {
    val s = allHidden(graph)

    graph.violations().foreach{ edge =>
      val path1 = graph.containerPath(edge.source)
      val path2 = graph.containerPath(edge.target)

      s.setVisibility(path1, Visible)
      s.setVisibility(path2, Visible)
    }
    s
  }

}
class VisibilitySet private (private [this] var hiddens: Set[NodeId])/*(val graph : AccessGraph)*/ {

  def setVisibility(id : NodeId, v : Visibility) : Unit = v match {
    case Visible => hiddens -= id
    case Hidden => hiddens += id
  }

  def setVisibility(ids : Seq[NodeId], v : Visibility) : Unit = v match {
    case Visible => hiddens --= ids
    case Hidden => hiddens ++= ids
  }

  def toggle(id : NodeId): Unit =
    setVisibility(id, visibility(id).opposite)


  def isVisible(id:NodeId) : Boolean = !isHidden(id)
  def isHidden : NodeId => Boolean = hiddens.contains

  def visibility(id : NodeId): Visibility = {
     if(isHidden(id)) Hidden
     else Visible
  }
}
