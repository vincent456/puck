package puck.graph

/**
 * Created by lorilan on 07/05/14.
 */
class StatelessAGNode (graph: AccessGraph,
                       id: Int,
                       name: String,
                       kind: NodeKind,
                       `type`: Option[Type])
  extends AGNode(graph, id, name, kind){

  override def superTypes_+=(st:AGNode) = ()
  override def subTypes_+=(st:AGNode) = ()
  override def users_+=(n:AGNode) = ()
}
