package puck.graph

/**
 * Created by lorilan on 1/8/15.
 */

sealed trait DGNode{
  val id : NodeId
  val name : String
  //val kind : NodeKind
  //val styp : TypeHolder
  val isMutable : Boolean

}

object DGNode {
  def apply(id : NodeId,
            name : String,
            kind : NodeKind,
            styp : TypeHolder,
            isMutable : Boolean) : DGNode = ConcreteNode(id, name, kind, styp, isMutable)
}

case class VirtualNode
(id : NodeId,
 name : String,
 /*kind : NodeKind,
 styp : TypeHolder,*/
 potentialMatches : Set[ConcreteNode]) extends DGNode {
  override val isMutable = true

  def kinds : Set[NodeKind]=
    potentialMatches.foldLeft(Set[NodeKind]()){(s,n) =>
      s + n.kind
    }
}

case class ConcreteNode
( id : NodeId,
  name : String,
  kind : NodeKind,
  styp : TypeHolder,
  isMutable : Boolean)  extends DGNode {

  override def toString = id + " - " + kind +" " + name


  /*def distance(other : AGNode[Kind]) = {
    if(this == other) 0
    else if(this contains_*  other)
      other.containerPath(this).length - 1
    else if (other contains_* this)
      this.containerPath(other).length  - 1
    else {
      val thisPathToRoot = this.containerPath.reverse
      val otherPathToRoot = other.containerPath.reverse
      println(thisPathToRoot)
      println(otherPathToRoot)
      thisPathToRoot.foldLeft[Option[AGNode[Kind]]](None) {
        case (sn@Some(_), _) => sn
        case (None, n) =>
          if (otherPathToRoot contains n)
            Some(n)
          else
            None

      } match {
        case None =>
          Int.MaxValue
        case Some(commonAncestor) =>
          val count: ((Int, Boolean), AGNode[Kind]) => (Int, Boolean) = {
            case ((i, alreadyFound), n) =>
              if (alreadyFound || n == commonAncestor) (i, true)
              else (i + 1, false)
          }

          (thisPathToRoot.foldLeft((0, false))(count),
            otherPathToRoot.foldLeft((0, false))(count)) match {
            case ((i, _), (j, _)) => i + j
          }
      }
    }

  }*/




}
