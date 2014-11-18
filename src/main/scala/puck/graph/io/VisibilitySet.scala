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
class VisibilitySet/*(val graph : AccessGraph)*/ {

  var hiddens = Set[NodeId]()

  def setVisibility(id : NodeId, v : Visibility) : Unit = v match {
    case Visible => hiddens -= id
    case Hidden => hiddens += id
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
