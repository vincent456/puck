package puck.graph.immutable

/**
 * Created by lorilan on 26/10/14.
 */

import puck.graph.constraints.AbstractionPolicy
import puck.graph.immutable.AccessGraph.{Mutability, NodeId}

trait AGNodeBuilder[Kind <: NodeKind[Kind]] {
  def apply(graph : AccessGraph[Kind],
            id : NodeId[Kind],
            name : String,
            kind : Kind,
            isMutable : Mutability) : AGNode[Kind]

  def rootKind : Kind
  def kinds : Seq[Kind]
}

class AGNode[Kind <: NodeKind[Kind]]
( val graph : AccessGraph[Kind],
  val id : NodeId[Kind],
  val name : String,
  val kind : Kind,
  val isMutable : Boolean){

  type NodeIdT = NodeId[Kind]

  def container = graph.container(id)
  def content = graph.content(id)

  def users = graph.users(id)
  def directSuperTypes = graph.directSuperTypes(id)

  def isRoot = graph.container(id) == id

  def isa( n : NodeIdT ) = graph.isa(id, n)

  def containerPath  : Seq[NodeIdT] = ???

  def abstractions :  Iterable[(NodeIdT, AbstractionPolicy)] = graph.abstractions(id)

  /*def canContain(n : NodeType) : Boolean = {
    n != this && !(n contains_* this) && // no cycle !
      (this.kind canContain n.kind) &&
      this.isMutable
  }*/


}
