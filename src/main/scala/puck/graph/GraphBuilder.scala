package puck.graph

import puck.graph.constraints._
import scala.collection.mutable
/**
 * Created by lorilan on 27/10/14.
 */

class GraphBuilder
( val nodeBuilder : AGNodeBuilder ){
  var g : AccessGraph = _
  type NodeIdT = NodeId
  val nodesByName = mutable.Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor

  def addPredefined(id : NodeIdT, fullName : String, name : String, kind : NodeKind, t : Hook): Unit ={
    g = g.addNode(id, name, kind, NoType, mutable = false, t)
    nodesByName += (fullName -> id)
  }

  def addNode(unambiguousFullName: String, localName:String, kind: NodeKind, th : TypeHolder): NodeIdT = {
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

  def setType(id : NodeIdT, typ : TypeHolder): Unit ={
    g = g.setType(id, typ)
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

  var constraintsMap = ConstraintsMaps()

  def discardConstraints(): Unit ={
    constraintsMap = ConstraintsMaps()
  }

  def setDefs(defs : Map[String, NamedNodeSet]): Unit = {
    constraintsMap = constraintsMap.newConstraintsMaps(nNamedSets =  defs)
  }

  def addScopeConstraint(owners : NodeSet,
                         facades : NodeSet,
                         interlopers : NodeSet,
                         friends : NodeSet) = {
    val ct = new ScopeConstraint(owners, facades, interlopers, friends)

    val scopeCtsMap = owners.foldLeft(constraintsMap.scopeConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, new ConstraintSet[ScopeConstraint]())
        map + (ownerId -> (s + ct) )
    }
    constraintsMap = constraintsMap.newConstraintsMaps(nScopeConstraints = scopeCtsMap)
  }

  def addElementConstraint(owners : NodeSet,
                           interlopers : NodeSet,
                           friends : NodeSet) = {
    val ct = new ElementConstraint(owners, interlopers, friends)

    val eltCtsMap = owners.foldLeft(constraintsMap.elementsConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, new ConstraintSet[ElementConstraint]())
        map + (ownerId -> (s + ct) )
    }

    constraintsMap = constraintsMap.newConstraintsMaps(nElementsConstraints = eltCtsMap)

  }

  def addScopeFriendOfScopeConstraint( friends : NodeSet,
                           befriended : NodeSet) = {
    val ct = new ScopeFriendOfScopesConstraint(friends, befriended)

    val friendCtsMap = befriended.foldLeft(constraintsMap.friendOfScopesConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, new ConstraintSet[ScopeFriendOfScopesConstraint]())
        map + (ownerId -> (s + ct) )
    }

    constraintsMap = constraintsMap.newConstraintsMaps(nFriendOfScopesConstraints = friendCtsMap)

  }

  def addScopeFriendOfElementConstraint( friends : NodeSet,
                                befriended : NodeSet) = {
    val ct = new ScopeFriendOfElementsConstraint(friends, befriended)

    val friendCtsMap = befriended.foldLeft(constraintsMap.friendOfElementsConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, new ConstraintSet[ScopeFriendOfElementsConstraint]())
        map + (ownerId -> (s + ct) )
    }

    constraintsMap = constraintsMap.newConstraintsMaps(nFriendsOfElementsConstraints = friendCtsMap)

  }

  def addCanSee( friends : NodeSet,
                 befriended : NodeSet) = {
    val ct = new ElementFriendOfElementsConstraint(friends, befriended)

    val friendCtsMap = befriended.foldLeft(constraintsMap.canSeeConstraints){
      case (map, ownerId) =>
        val s = map.getOrElse(ownerId, CanSeeSet())
        map + (ownerId -> (s + ct) )
    }

    constraintsMap = constraintsMap.newConstraintsMaps(nCanSeeConstraints = friendCtsMap)

  }
}
