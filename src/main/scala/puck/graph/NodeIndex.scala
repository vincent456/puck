package puck.graph


sealed trait NodeStatus
case object Removed extends NodeStatus
case object Created extends NodeStatus

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
      Nodes2VNodeMap()).addConcreteNode(root)
}

import NodeIndex._
case class NodeIndex
( idSeed : Int,
  cNodes : ConcreteNodeIndex,
  removedCnodes : ConcreteNodeIndex,
  vNodes : VirtualNodeIndex,
  removedVnodes : VirtualNodeIndex,
  cNodes2vNodes : Nodes2VnodeMap){

  private [graph] def addConcreteNode(n : ConcreteNode) : NodeIndex =
    copy(cNodes = cNodes + (n.id -> n),
      removedCnodes = removedCnodes - n.id)

  def addConcreteNode
  ( localName : String,
    kind : NodeKind,
    th : Option[Type],
    mutable : Mutability = true
    ) : (ConcreteNode, NodeIndex) = {
    val n = ConcreteNode(idSeed + 1, localName, kind, th, mutable)
    (n, copy(idSeed = idSeed + 1).addConcreteNode(n))
  }

  private [graph] def addVirtualNode
  ( n : VirtualNode ) : NodeIndex =
    copy(vNodes = vNodes + (n.id -> n),
      removedVnodes = removedVnodes - n.id,
      cNodes2vNodes = cNodes2vNodes + (n.potentialMatches -> n.id))

  def addVirtualNode(ns : Seq[NodeId], k : NodeKind) : (VirtualNode, NodeIndex) = {
    cNodes2vNodes get ns match {
      case Some(vnid) => (vNodes(vnid), this)
      case None => val n = VirtualNode(idSeed + 1, ns, k)
        (n, copy(idSeed = idSeed + 1).addVirtualNode(n))
    }
  }

  def nodes : Iterable[DGNode] = vNodes.values ++ cNodes.values
  def concreteNodes : Iterable[ConcreteNode] = cNodes.values
  def virtualNodes : Iterable[VirtualNode] = vNodes.values

  def nodesId : Iterable[NodeId] = vNodes.keys ++ cNodes.keys
  def concreteNodesId : Iterable[NodeId] = cNodes.keys

  def numNodes : Int = cNodes.size + vNodes.size
  def numRemovedNodes : Int = removedCnodes .size + removedVnodes.size

  def getConcreteNodeWithStatus(id : NodeId): (ConcreteNode, NodeStatus) =
    cNodes get id map {(_, Created)} getOrElse{
      removedCnodes get id map {(_, Removed)} getOrElse {
        val msg =
          s"AccessGraph.getNode : no concrete node has id ${id.toString}\n" +
            s"concrete nodes :${cNodes.toSeq.sortBy(_._1).map(_._2).mkString("\n\t", "\n\t", "\n")}" +
            s"virtual nodes :${vNodes.toSeq.sortBy(_._1).map(_._2).mkString("\n\t", "\n\t", "")}"
        throw new DGError(msg)
      }
    }

  def getNode(id : NodeId): DGNode = try {
    getConcreteNodeWithStatus(id)._1
  } catch {
    case e : DGError => vNodes(id)
  }

  def getConcreteNode(id : NodeId): ConcreteNode =
    getConcreteNodeWithStatus(id)._1


  def removeConcreteNode(id : NodeId) : (ConcreteNode, NodeIndex) = {
    val n = cNodes(id)
    if(n.id != id)
      throw new DGError("incoherent index left and right id are different")
    (n, copy(cNodes = cNodes - id,
      removedCnodes = removedCnodes + (id -> n)))
  }

  def removeVirtualNode(id : NodeId) : (VirtualNode, NodeIndex) = {
    val n = vNodes(id)
    if(n.id != id)
      throw new DGError("incoherent index left and right id are different")
    (n, copy(vNodes = vNodes - n.id,
      removedVnodes = removedVnodes + (n.id -> n),
      cNodes2vNodes = cNodes2vNodes + (n.potentialMatches -> n.id)))
  }


  private def setNode(n : DGNode, s : NodeStatus) : NodeIndex = (n, s) match {
    case (cn: ConcreteNode, Created) =>
      copy(cNodes = cNodes + (n.id -> cn) )
    case (cn : ConcreteNode, Removed) =>
      copy(removedCnodes = removedCnodes + (n.id -> cn) )
    case (vn: VirtualNode, Created) =>
      copy(vNodes = vNodes + (n.id -> vn) )
    case (vn : VirtualNode, Removed) =>
      copy(removedVnodes = removedVnodes + (n.id -> vn) )
  }

  def setName(id : NodeId, newName : String) : (String, NodeIndex) = {
    val (n, s) = getConcreteNodeWithStatus(id)
    (n.name, setNode(n.copy(name = newName), s))
  }

  def setType(id : NodeId, st : Option[Type]) : NodeIndex = {
    val (n, s) = getConcreteNodeWithStatus(id)
    setNode(n.copy(styp = st), s)
  }

  def setMutability(id : NodeId, mutable : Boolean) : NodeIndex =  {
    val (n, s) = getConcreteNodeWithStatus(id)
    setNode(n.copy(mutable = mutable), s)
  }
}
