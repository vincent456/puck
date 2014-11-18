package puck.graph.io

import puck.graph.NodeId

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
  def apply(ids : NodeId*) = new VisibilitySet(Set()++ ids.toSeq)

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
