/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph


sealed trait NodeStatus
case object Removed extends NodeStatus
case object Existing extends NodeStatus

object NodeIndex {

  type ConcreteNodeIndex = Map[NodeId, ConcreteNode]
  val ConcreteNodeIndex = Map
  type VirtualNodeIndex = Map[NodeId, VirtualNode]
  val VirtualNodeINdex = Map
  type Nodes2VnodeMap = Map[Set[NodeId], NodeId]
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

  def addVirtualNode(ns : Set[NodeId], k : NodeKind) : (VirtualNode, NodeIndex) = {
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
