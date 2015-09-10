package puck.graph


sealed trait NodeStatus
case object Removed extends NodeStatus
case object Existing extends NodeStatus

object NodeIndex {

  type ConcreteNodeIndex = Map[NodeId, ConcreteNode]
  val ConcreteNodeIndex = Map
  type VirtualNodeIndex = Map[NodeId, VirtualNode]
  val VirtualNodeINdex = Map
  type Nodes2VnodeMap = Map[Seq[NodeId], NodeId]
  val Nodes2VNodeMap = Map

  def apply(root : ConcreteNode) : NodeIndex =
    NodeIndex(root.id,
      ConcreteNodeIndex() + (root.id -> root), ConcreteNodeIndex(),
      VirtualNodeINdex(), VirtualNodeINdex(),
      Nodes2VNodeMap(), Map()).addConcreteNode(root)

  def getNodeWithStatus[N <: DGNode](id : NodeId,
                                     nodes : Map[NodeId, N],
                                     removedNodes : Map[NodeId, N]) : (N, NodeStatus) =
    nodes get id map {(_, Existing)} getOrElse{
      removedNodes get id map {(_, Removed)} getOrElse {
        val msg =
          s"AccessGraph.getNode : no node has id ${id.toString}\n" +
            s"nodes :${nodes.toSeq.sortBy(_._1).map(_._2).mkString("\n\t", "\n\t", "\n")}" +
            s"removed nodes :${removedNodes.toSeq.sortBy(_._1).map(_._2).mkString("\n\t", "\n\t", "")}"
        throw new DGError(msg)
      }
    }
}

import NodeIndex._
case class NodeIndex
( idSeed : Int,
  cNodes : ConcreteNodeIndex,
  removedCnodes : ConcreteNodeIndex,
  vNodes : VirtualNodeIndex,
  removedVnodes : VirtualNodeIndex,
  cNodes2vNodes : Nodes2VnodeMap,
  roles : Map[NodeId, Role]){

  private [this] def adjustSeed(nid : NodeId) =
    if(nid <= idSeed) this
    else copy(idSeed = nid)

  private [graph] def addConcreteNode
  (n : ConcreteNode) : NodeIndex =
    adjustSeed(n.id).
      copy(cNodes = cNodes + (n.id -> n),
      removedCnodes = removedCnodes - n.id)

  def addConcreteNode
  ( localName : String,
    kind : NodeKind,
    mutable : Mutability = true
    ) : (ConcreteNode, NodeIndex) = {
    val n = ConcreteNode(idSeed + 1, localName, kind, mutable)
    (n, addConcreteNode(n))
  }

  private [graph] def addVirtualNode
  ( n : VirtualNode ) : NodeIndex =
    adjustSeed(n.id).
      copy(vNodes = vNodes + (n.id -> n),
      removedVnodes = removedVnodes - n.id,
      cNodes2vNodes = cNodes2vNodes + (n.potentialMatches -> n.id))

  def addVirtualNode(ns : Seq[NodeId], k : NodeKind) : (VirtualNode, NodeIndex) = {
    cNodes2vNodes get ns match {
      case Some(vnid) =>
        vNodes get vnid map ((_, this)) getOrElse {
          val vn = removedVnodes(vnid)
          (vn, addVirtualNode(vn))
        }

      case None => val n = VirtualNode(idSeed + 1, ns, k)
        (n, addVirtualNode(n))
    }
  }

  def removeConcreteNode(n : ConcreteNode) : NodeIndex =
    copy(cNodes = cNodes - n.id,
      removedCnodes = removedCnodes + (n.id -> n))


  def removeVirtualNode(n : VirtualNode) : NodeIndex =
    copy(vNodes = vNodes - n.id,
      removedVnodes = removedVnodes + (n.id -> n),
      cNodes2vNodes = cNodes2vNodes + (n.potentialMatches -> n.id))


  def nodes : Iterable[DGNode] = vNodes.values ++ cNodes.values
  def concreteNodes : Iterable[ConcreteNode] = cNodes.values
  def virtualNodes : Iterable[VirtualNode] = vNodes.values

  def nodesId : Iterable[NodeId] = vNodes.keys ++ cNodes.keys
  //def removedNodesId : Iterable[NodeId] = removedCnodes.keys ++ removedVnodes.keys
  def concreteNodesId : Iterable[NodeId] = cNodes.keys

  def highestId : Int =
    (vNodes.keys ++ cNodes.keys ++ removedCnodes.keys ++ removedVnodes.keys).fold(-1)(Math.max)

  def numNodes : Int = cNodes.size + vNodes.size
  def numRemovedNodes : Int = removedCnodes .size + removedVnodes.size


  def getConcreteNodeWithStatus(id : NodeId): (ConcreteNode, NodeStatus) =
      getNodeWithStatus(id, cNodes, removedCnodes)

  def getVirtualNodeWithStatus(id : NodeId): (VirtualNode, NodeStatus) =
      getNodeWithStatus(id, vNodes, removedVnodes)

  def getNode(id : NodeId): DGNode = try {
    getConcreteNodeWithStatus(id)._1
  } catch {
    case e : DGError =>
      try {
        getVirtualNodeWithStatus(id)._1
      } catch {
        case e1 : DGError => throw new DGError(e.getMessage + e1.getMessage)
      }
  }

  def getConcreteNode(id : NodeId): ConcreteNode =
    getConcreteNodeWithStatus(id)._1


  private def setNode(n : DGNode, s : NodeStatus) : NodeIndex = (n, s) match {
    case (cn: ConcreteNode, Existing) =>
      copy(cNodes = cNodes + (n.id -> cn) )
    case (cn : ConcreteNode, Removed) =>
      copy(removedCnodes = removedCnodes + (n.id -> cn) )
    case (vn: VirtualNode, Existing) =>
      copy(vNodes = vNodes + (n.id -> vn) )
    case (vn : VirtualNode, Removed) =>
      copy(removedVnodes = removedVnodes + (n.id -> vn) )
  }

  def setName(id : NodeId, newName : String) : (String, NodeIndex) = {
    val (n, s) = getConcreteNodeWithStatus(id)
    (n.name, setNode(n.copy(name = newName), s))
  }

  def getRole(id : NodeId) : Option[Role] =
    roles get id

  def setRole(id : NodeId, srole : Option[Role]) : NodeIndex =
    srole match {
      case None =>
        if(roles contains id) copy(roles = roles - id)
        else this
      case Some(role) => copy(roles = roles + (id -> role))
    }


  def removeRole(id : NodeId) : NodeIndex =
    copy(roles = roles - id)


  def setMutability(id : NodeId, mutable : Boolean) : NodeIndex =  {
    val (n, s) = getConcreteNodeWithStatus(id)
    setNode(n.copy(mutable = mutable), s)
  }
}
