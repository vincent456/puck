package puck.graph

import puck.graph.constraints._
/**
 * Created by lorilan on 27/10/14.
 */

class GraphBuilder {
  var g : DependencyGraph = _
  type NodeIdT = NodeId
  var nodesByName = Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor

  def addPredefined(id : NodeIdT, fullName : String, name : String, kind : NodeKind): Unit = {
    g = g.addConcreteNode(name, kind, None, mutable = false, Some(id))._2
    nodesByName += (fullName -> id)
    ()
  }

  def addNode(unambiguousFullName: String, localName:String, kind: NodeKind, th : Option[Type]): NodeIdT = {
    nodesByName get unambiguousFullName match {
      case None =>
        val (n, g2) = g.addConcreteNode(localName, kind, th)
        this.nodesByName += (unambiguousFullName -> n.id)
        g = g2
        n.id
      case Some(id) => id /* check that the kind and type is indeed the same ??*/
    }
  }

  def setMutability(id : NodeIdT, mutable : Boolean): Unit ={
    g = g.setMutability(id, mutable)
  }

  def setType(id : NodeIdT, typ : Option[Type]): Unit ={
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

  def addTypeRelationship(typeUser: NodeIdT, typeUsed: NodeIdT,
                         typeMemberUser: NodeIdT, typeMemberUsed:NodeIdT): Unit ={
    g = g.addUsesDependency( (typeUser, typeUsed),
                              (typeMemberUser, typeMemberUsed))
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

  type ImplId = NodeId
  type AbsId = NodeId

  def registerAbstraction : DependencyGraph => (ImplId, AbsId, AbstractionPolicy) => DependencyGraph =
    graph => (implId , absId, pol) => graph.addAbstraction(implId, (absId, pol))


  def registerSuperTypes() =
    g = g.isaEdges.foldLeft(g){ (g, e) =>
      registerAbstraction(g)(e.source, e.target, SupertypeAbstraction)
    }

}
