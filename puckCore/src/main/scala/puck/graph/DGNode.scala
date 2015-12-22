package puck.graph

sealed trait DGNode{
  val id : NodeId
  val name : String
  val kind : NodeKind
  //def styp(g : DependencyGraph) : Option[Type]
  val mutable : Boolean

  def mapConcrete[A](f : ConcreteNode => A, default : => A) : A
  def name(g : DependencyGraph) : String

  def definition(g : DependencyGraph) : Option[NodeId] = {
    kind.kindType match {
      case InstanceValueDecl
           | StaticValueDecl =>
        g definitionOf this.id
      case _ => None
    }
  }

  def hasDefinitionIn(g : DependencyGraph) : Boolean =
    (g definitionOf this.id).nonEmpty

  def definition_!(g : DependencyGraph) : NodeId =
    g definitionOf_! this.id


}

object DGNode {
  def apply(id : NodeId,
            name : String,
            kind : NodeKind,
            styp : Option[Type],
            isMutable : Boolean) : DGNode = ConcreteNode(id, name, kind, isMutable)
}

case class VirtualNode
(id : NodeId,
 potentialMatches : Set[NodeId],
 kind : NodeKind) extends DGNode {
  val mutable = true
  //def styp(g : DependencyGraph) = None
  //def name : String =  potentialMatches mkString ("Virtual(", " \\/ ", ")")

  def mapConcrete[A](f : ConcreteNode => A, default : => A) : A = default

  private def mkNameString(stringify : NodeId => String) =
    potentialMatches map stringify mkString ("Virtual(", " \\/ ", ")")

  val name : String = mkNameString(_.toString)
  def name(g : DependencyGraph) : String =mkNameString(g.getNode(_).name)


}

case class ConcreteNode
( id : NodeId,
  name : String,
  kind : NodeKind,
  mutable : Boolean)  extends DGNode {

  //def styp(g : DependencyGraph) = ???/* g styp id*/

  def name(g : DependencyGraph) : String = name

  override def toString = {
    val non = if(mutable) ""
    else "non-"
    s"($id - $kind $name ${non}mutable)"
  }


  def mapConcrete[A](f : ConcreteNode => A, default : => A) : A = f(this)



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
