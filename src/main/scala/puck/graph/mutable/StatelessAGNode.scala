package puck.graph.mutable

import puck.graph.mutable

import scala.collection.{mutable => smutable}
import scala.language.implicitConversions
/**
 * Created by lorilan on 07/05/14.
 */
class StatelessAGNode[Kind <: NodeKind[Kind]] (graph: AccessGraph[Kind],
                                               id: Int,
                                               name: String,
                                               kind: Kind)
  extends AGNode[Kind](graph, id, name, kind){

  override def superTypes_+=(st:AGNode[Kind], register : Boolean) = ()
  //TODO find a solution to make it stateless again
  //override def users_+=(n:AGNode[Kind], register : Boolean) = ()

  override val users = stateLessUsersObject

  object stateLessUsersObject {
    def apply() : smutable.Iterable[NodeType] = smutable.Iterable.empty

    implicit def iterableUsers(c : StatelessAGNode.this.users.type ) : smutable.Iterable[NodeType] = smutable.Iterable.empty
    def contains(n : NodeType) = false

    def +=(user : NodeType, register : Boolean = true) {}

    def -=(user : NodeType, register : Boolean = true) {}
  }
}
