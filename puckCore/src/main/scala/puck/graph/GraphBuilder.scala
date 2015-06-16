package puck.graph

import puck.graph.constraints._

class GraphBuilder {
  var g : DependencyGraph = _
  type NodeIdT = NodeId
  var nodesByName = Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor

  def getFullName( id : NodeIdT) : String = g.fullName(id)

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

  def addContains(containerId : NodeIdT, contentId : NodeIdT): Unit ={
    g = g.addContains(containerId, contentId)
  }

  def addEdge(e : DGEdge): Unit ={
    g = g.addEdge(e)
  }

  def addIsa(subTypeId: NodeIdT, superTypeId: NodeIdT): Unit = {
    g = g.addIsa(subTypeId, superTypeId)
  }

   def addTypeRelationship(typeUse : DGUses,
                           typeMemberUse : DGUses): Unit ={
    g = g.addUsesDependency(typeUse, typeMemberUse)
  }

  type ImplId = NodeId
  type AbsId = NodeId

  def registerAbstraction : DependencyGraph => (ImplId, Abstraction) => DependencyGraph =
    graph => (implId , abs) => graph.addAbstraction(implId, abs)


  def registerSuperTypes() =
    g = g.isaEdges.foldLeft(g){ (g, e) =>
      registerAbstraction(g)(e.source, AccessAbstraction(e.target, SupertypeAbstraction))
    }

}
