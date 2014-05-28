package puck.graph

/**
 * Created by lorilan on 16/05/14.
 */

sealed abstract class EdgeKind
case class Uses() extends EdgeKind
case class Contains() extends EdgeKind
case class Isa() extends EdgeKind

class AGEdge(val kind : EdgeKind, val source : AGNode, val target: AGNode)

object AGEdge{
  def apply(kind : EdgeKind,source : AGNode,target: AGNode) = new AGEdge(kind, source, target)
  def uses(source : AGNode,target: AGNode) = apply(Uses(), source, target)
  def contains(source : AGNode,target: AGNode) = apply(Contains(), source, target)
}
