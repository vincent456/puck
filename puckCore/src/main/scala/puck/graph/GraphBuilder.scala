package puck.graph

import puck.graph.constraints.SupertypeAbstraction

trait GraphBuilder {
  var g : DependencyGraph = _
  type NodeIdT = NodeId
  var nodesByName = Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor

  def getFullName( id : NodeIdT) : String = g.fullName(id)

  def addNode(unambiguousFullName: String, localName:String, kind: NodeKind): NodeIdT = {
    nodesByName get unambiguousFullName match {
      case None =>
        val (n, g2) = g.addConcreteNode(localName, kind)
        //println(s"adding ${n.id} $unambiguousFullName ")
        this.nodesByName += (unambiguousFullName -> n.id)
        g = g2
        n.id
      case Some(id) => id /* check that the kind and type is indeed the same ??*/
    }
  }

  private val anonymous = "Anonymous"
  def addAnonymousNode(kind : NodeKind) : NodeIdT = {
    val (n, g2) = g.addConcreteNode(anonymous, kind)
    g = g2
    n.id
  }



  def setMutability(id : NodeIdT, mutable : Boolean): Unit ={
    g = g.setMutability(id, mutable)
  }

  def setType(id : NodeIdT, typ : Type): Unit ={
    g = g.setType(id, Some(typ))
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

  def addDef(decl : NodeId, _def : NodeId) : Unit = {
    g = g.addDef(decl, _def)
  }
  def addParams(decl : NodeId, params : List[Int]) : Unit = {
    params.reverseIterator.foreach{
      param =>
        g = g.addParam(decl, param)
    }
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
