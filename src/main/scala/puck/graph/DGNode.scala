package puck.graph

/**
 * Created by lorilan on 1/8/15.
 */



case class DGNode
( id : NodeId,
  name : String,
  kind : NodeKind,
  styp : TypeHolder,
  isMutable : Boolean,
  status : NodeStatus){

  type NIdT = NodeId
  type GraphT = DependencyGraph


  override def toString = id + " - " + kind +" " + name

  /*def container(implicit graph : GraphT) = graph.container(id)
  def content(implicit graph : GraphT) = graph.content(id)

  def users(implicit graph : GraphT) = graph.users(id)
  def used(implicit graph : GraphT) = graph.usedBy(id)

  def directSuperTypes(implicit graph : GraphT) = graph.directSuperTypes(id)
  def directSubTypes(implicit graph : GraphT) = graph.directSubTypes(id)

  def subTypes(implicit graph : GraphT) = graph.subTypes(id)
  def isSuperTypeOf(subCandidate : NIdT)(implicit graph : GraphT) = graph.isSuperTypeOf(id, subCandidate)

  def isRoot(implicit graph : GraphT) = graph.isRoot(id)

  def isa( n : NIdT )(implicit graph : GraphT) = graph.isa(id, n)



  def abstractions(implicit graph : GraphT) :  Iterable[(NIdT, AbstractionPolicy)] = graph.abstractions(id)
  */



  def wrongUsers(implicit graph : GraphT) : Seq[NIdT] = graph.wrongUsers(id)
  def isWronglyContained(implicit graph : GraphT) : Boolean = graph.isWronglyContained(id)



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


  private def outgoingDependencies(root : NIdT, acc0 : Set[DGEdge])(implicit graph : GraphT) : Set[DGEdge]= {
    val acc1 = graph.usedBy(id).foldLeft(acc0){
      (acc, usee) =>
        if(graph.contains_*(root,usee)) acc
        else acc + DGEdge.uses(this.id, usee)
    }
    graph.content(id).foldLeft(acc1){(acc, child) => graph.getNode(child).outgoingDependencies(root, acc)}
  }

  def outgoingDependencies(implicit graph : GraphT) : Set[DGEdge] = outgoingDependencies(this.id, Set[DGEdge]())

  private def incomingDependencies(root : NIdT, acc0 : Set[DGEdge])(implicit graph : GraphT) : Set[DGEdge]= {
    val acc1 = graph.users(id).foldLeft(acc0){
      (acc, user) =>
        if(graph.contains_*(root, user)) acc
        else acc + DGEdge.uses(user, this.id)
    }
    graph.content(id).foldLeft(acc1){(acc, child) => graph.getNode(child).incomingDependencies(root, acc)}
  }

  def incomingDependencies(implicit graph : GraphT) : Set[DGEdge] = incomingDependencies(this.id, Set[DGEdge]())


  private def internalDependencies(root : NIdT, acc0 : Set[DGEdge])(implicit graph : GraphT) : Set[DGEdge]= {

    val acc1 = graph.usedBy(id).foldLeft(acc0) {
      (acc, usee) =>
        if (graph.contains_*(root,usee))
          acc + DGEdge.uses(id, usee)
        else acc
    }
    /* not necessary
        val acc2 = users.foldLeft(acc1){
          (acc, user) =>
            if(root contains_* user)
              acc + AGEdge.uses(user, this)
            else acc
        }
    */

    graph.content(id).foldLeft(acc1){(acc, child) => graph.getNode(child).internalDependencies(root, acc)}
  }

  def internalDependencies(implicit graph : GraphT) : Set[DGEdge] = internalDependencies(this.id, Set[DGEdge]())


  /*def provides(other : AGNode) = {
    val these0 = Set[AGNode]() ++ this.iterator
    val others0 = Set[AGNode]() ++ other.iterator

    val these = these0 -- others0
    val others = others0 -- these0
    these.exists { t => others.exists(o => o uses t) }
  }*/


  def provides(other : NIdT)(implicit graph : GraphT) = {
    val these = graph.subTree(id)
    val others = graph.subTree(other)

    these.exists { t =>
      others.exists { o =>
        graph.uses(o, t) &&
          !(graph.contains_*(other, o) && graph.contains_*(other, t))
      }
    }
  }

  private def connection(f : DGNode => Boolean)(implicit graph : GraphT) = {
    graph.nodes.foldLeft(Set[NIdT]()){ (acc, n) =>
      if(n.id == this.id || n.kind != this.kind) acc
      else if(f(n)) acc + n.id
      else acc
    }
  }

  def providers(implicit graph : GraphT) : Set[NIdT] = connection{ n => n provides this.id}
  def clients(implicit graph : GraphT) : Set[NIdT] = connection{ n => this provides n.id}

  def cohesion(implicit graph : GraphT) : Double = {
    val intd = internalDependencies.size
    intd.toDouble / (outgoingDependencies.size + incomingDependencies.size + intd).toDouble
  }

  def coupling(implicit graph : GraphT) : Double = {
    val dependencies = outgoingDependencies.size + incomingDependencies.size + internalDependencies.size
    1 - (providers ++ clients).size.toDouble / dependencies.toDouble
  }

}
