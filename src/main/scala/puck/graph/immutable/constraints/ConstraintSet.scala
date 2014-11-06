package puck.graph.immutable.constraints

import puck.graph.immutable.{NodeKind, AccessGraph}
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 11/06/14.
 */

object ConstraintSet{
  def empty[T <: Constraint] =
    new ConstraintSet[T](Seq())
}
class ConstraintSet[T <: Constraint]
(private val content : Seq[T])
  extends Iterable[T]{

  def this() = this(Seq[T]())

  override def toString = content.mkString("\n")

  // /!\ uses eq and not ==
  def replaceEq(ct : T, newCt : T) : ConstraintSet[T] =
    new ConstraintSet(content.map {ct0 => if(ct0 eq ct) newCt else ct0})

  def replaceEq(cts : Seq[(T, T)]) : ConstraintSet[T] =
    cts.foldLeft(this){
      case (m, (oldCt, newCt)) => m.replaceEq(oldCt, newCt)
    }

  def iterator = content.iterator

  def + (ct : T) : ConstraintSet[T] = new ConstraintSet( ct +: content )
  //def - (ct : T) : ConstraintSet[Kind, T] = new ConstraintSet( content - ct )

  def friendScopeThatContains_*(graph : AccessGraph, n: NodeId) = {

    def aux(l : Seq[T]) : Option[NodeId] =
      if(l.isEmpty) None
      else{
        l.head.friends.scopeThatContains_*(graph, n) match {
          case None => aux(l.tail)
          case sn => sn
        }
      }

    aux(content)

  }
  def hasFriendScopeThatContains_*(graph : AccessGraph, n : NodeId)=
    content.exists( _.friends.hasScopeThatContains_*(graph, n))

}
