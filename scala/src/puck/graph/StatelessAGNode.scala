package puck.graph

/**
 * Created by lorilan on 07/05/14.
 */
class StatelessAGNode (graph: AccessGraph,
                       id: Int,
                       name: String,
                       kind: NodeKind,
                       `type`: Option[Type])
  extends AGNode(graph, id, name, kind, `type`){

  override def addSuperType(st:AGNode) = ()
  override def addSubType(st:AGNode) = ()
  override def addUser(n:AGNode) = ()
}
