package puck.graph.immutable

import AccessGraph.NodeId
import puck.graph.immutable.constraints._
import scala.collection.mutable
/**
 * Created by lorilan on 27/10/14.
 */

class GraphBuilder[Kind <: NodeKind[Kind], T]
( val nodeBuilder : AGNodeBuilder[Kind, T] ){
  var g : AccessGraph[Kind, T] = _
  type NodeIdT = NodeId[Kind]
  val nodesByName = mutable.Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor


  def addPredefined(id : NodeIdT, fullName : String, name : String, kind : Kind, t : T): Unit ={
    g = g.addNode(id, name, kind, NoType(), mutable = false, t)
    nodesByName += (fullName -> id)
  }

  def addNode(unambiguousFullName: String, localName:String, kind: Kind, th : TypeHolder[Kind]): NodeIdT = {
    nodesByName get unambiguousFullName match {
      case None =>
        val (id, g2) = g.addNode(localName, kind, th)
        this.nodesByName += (unambiguousFullName -> id)
        g = g2
        id
      case Some(id) => id /* check that the kind and type is indeed the same ??*/
    }
  }

  def setMutability(id : NodeIdT, mutable : Boolean): Unit ={
    g = g.setMutability(id, mutable)
  }

  def addContains(containerId: NodeIdT, contentId :NodeIdT): Unit ={
    g = g.addContains(containerId, contentId)
  }
  def addUses(userId: NodeIdT, useeId: NodeIdT): Unit ={
    g = g.addUses(userId, useeId)
  }

  def addIsa(subTypeId: NodeIdT, superTypeId: NodeIdT): Unit = {
    g = g.addIsa(subTypeId, superTypeId)
  }

  def addUsesDependency(dominantUser: NodeIdT, dominantUsee: NodeIdT,
                         dominatedUser: NodeIdT, dominatedUsee:NodeIdT): Unit ={
    g = g.addUsesDependency( (dominantUser, dominantUsee),
                              (dominatedUser, dominatedUsee))
  }

  var constraintsMap = ConstraintsMaps[Kind]()

  def discardConstraints(): Unit ={
    constraintsMap = ConstraintsMaps[Kind]()
  }

  def addScopeConstraint(owners : NodeSet[Kind],
                         facades : NodeSet[Kind],
                         interlopers : NodeSet[Kind],
                         friends : NodeSet[Kind]) = {
    val ct = new ScopeConstraint(owners, facades, interlopers, friends)

    val scopeCtsMap = owners.foldLeft(constraintsMap.scopeConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, new ConstraintSet[Kind, ScopeConstraint[Kind]]())
        map + (ownerId -> (s + ct) )
    }
    constraintsMap = constraintsMap.newConstraintsMaps(nScopeConstraints = scopeCtsMap)
  }

  def addElementConstraint(owners : NodeSet[Kind],
                           interlopers : NodeSet[Kind],
                           friends : NodeSet[Kind]) = {
    val ct = new ElementConstraint(owners, interlopers, friends)

    val eltCtsMap = owners.foldLeft(constraintsMap.elementsConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, new ConstraintSet[Kind, ElementConstraint[Kind]]())
        map + (ownerId -> (s + ct) )
    }

    constraintsMap = constraintsMap.newConstraintsMaps(nElementsConstraints = eltCtsMap)

  }

  def addFriendConstraint(befriended : NodeSet[Kind],
                          friends : NodeSet[Kind]) = {
    val ct = new FriendConstraint[Kind](friends, befriended)

    val friendCtsMap = befriended.foldLeft(constraintsMap.friendConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, new ConstraintSet[Kind, FriendConstraint[Kind]]())
        map + (ownerId -> (s + ct) )
    }

    constraintsMap = constraintsMap.newConstraintsMaps(nFriendConstraints = friendCtsMap)

  }
}
