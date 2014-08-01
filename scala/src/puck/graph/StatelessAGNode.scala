package puck.graph

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
}
