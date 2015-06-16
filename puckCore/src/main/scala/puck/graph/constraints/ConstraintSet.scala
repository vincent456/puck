package puck.graph
package constraints

/**
 * Created by lorilan on 11/06/14.
 */

object ConstraintSet{
  def empty = new ConstraintSet(Seq())
}
class ConstraintSet
(private[constraints] val content : Seq[Constraint])
  extends Iterable[Constraint]{

  def this() = this(Seq[Constraint]())

  // /!\ uses eq and not ==
  def replaceEq(ct : Constraint, newCt : Constraint) : ConstraintSet =
    new ConstraintSet(content.map {ct0 => if(ct0 eq ct) newCt else ct0})

  def replaceEq(cts : Seq[(Constraint, Constraint)]) : ConstraintSet =
    cts.foldLeft(this){
      case (m, (oldCt, newCt)) => m.replaceEq(oldCt, newCt)
    }

  def iterator = content.iterator

  def + (ct : Constraint) : ConstraintSet = new ConstraintSet( ct +: content )
  //def - (ct : Constraint) : ConstraintSet[Kind, Constraint] = new ConstraintSet( content - ct )

  def hasFriendRangeThatContains_*(graph : DependencyGraph, n : NodeId)=
    content.exists( _.friends.hasRangeThatContains_*(graph, n))

}