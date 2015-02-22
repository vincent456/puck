package puck.graph

import puck.graph.constraints._
import scala.collection.mutable
/**
 * Created by lorilan on 27/10/14.
 */

class GraphBuilder
( val nodeBuilder : AGNodeBuilder ){
  var g : DependencyGraph = _
  type NodeIdT = NodeId
  val nodesByName = mutable.Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor

  def addPredefined(id : NodeIdT, fullName : String, name : String, kind : NodeKind): Unit ={
    g = g.addNode(id, name, kind, NoType, mutable = false)
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

  def setDefs(defs : Map[String, NamedRangeSet]): Unit = {
    constraintsMap = constraintsMap.copy(namedSets =  defs)
  }

  def addHideConstraint(owners : RangeSet,
                        facades : RangeSet,
                        interlopers : RangeSet,
                        friends : RangeSet) = {
    val ct = new Constraint(owners, facades, interlopers, friends)

    val hideConstraintsMap = owners.foldLeft(constraintsMap.hideConstraints){
      case (map, owner) =>
        val s = map.getOrElse(owner, new ConstraintSet())
        map + (owner -> (s + ct) )
    }
    constraintsMap = constraintsMap.copy(hideConstraints = hideConstraintsMap)
  }

  def addFriendConstraint( friends : RangeSet,
                           befriended : RangeSet) = {
    val ct = new Constraint(befriended, RangeSet.empty(), RangeSet.empty(), friends)

    val friendCtsMap = befriended.foldLeft(constraintsMap.friendConstraints){
      case (map, owner) =>
        val s = map.getOrElse(owner, new ConstraintSet())
        map + (owner -> (s + ct) )
    }

    constraintsMap = constraintsMap.copy(friendConstraints = friendCtsMap)

  }

}
