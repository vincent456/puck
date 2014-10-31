package puck.graph.immutable

import AccessGraph.NodeId
import scala.collection.mutable
/**
 * Created by lorilan on 27/10/14.
 */

class GraphBuilder[Kind <: NodeKind[Kind]]
( val nodeBuilder : AGNodeBuilder[Kind] ){
  var g : AccessGraph[Kind] = _
  type NodeIdT = NodeId[Kind]
  val nodesByName = mutable.Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor

  def addPredefined(fullName : String, name : String, kind : Kind): Unit ={
    g = g.addNode(kind.node, name, kind)
    nodesByName += (fullName -> kind.node)
  }

  def addNode(unambiguousFullName: String, localName:String, kind: Kind): NodeIdT = {
    nodesByName get unambiguousFullName match {
      case None =>
        val n = g.addNode(localName, kind)
        this.nodesByName += (unambiguousFullName -> n.id)
        g = n.graph
        n.id
      case Some(id) => id /* check that the kind and type is indeed the same ??*/
    }
  }

  def setMutability(id : NodeIdT, mutable : Boolean): Unit ={
    val node = g.setMutability(id, mutable)
    g = node.graph
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
}
