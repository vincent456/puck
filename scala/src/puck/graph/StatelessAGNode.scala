package puck.graph

import scala.collection.mutable
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
    def apply() : mutable.Iterable[NodeType] = mutable.Iterable.empty

    implicit def iterableUsers(c : StatelessAGNode.this.users.type ) : mutable.Iterable[NodeType] = mutable.Iterable.empty
    def contains(n : NodeType) = false

    def +=(user : NodeType, register : Boolean = true) {}

    def -=(user : NodeType, register : Boolean = true) {}
  }
}
