package puck.graph

/**
 * Created by lorilan on 1/8/15.
 */
trait TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: DGNode) : TypeHolder
  def redirectContravariantUses(oldUsee : NodeId, newUsee: DGNode) : TypeHolder
  def getTypeNodeIds : List[NodeId]
  def mkString(graph : DependencyGraph) : String
  def isEmpty = false

}
case object NoType extends TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: DGNode) = this
  def redirectContravariantUses(oldUsee : NodeId, newUsee: DGNode) = this

  def getTypeNodeIds : List[NodeId] = List()

  def mkString(graph : DependencyGraph) : String = ""
  override def isEmpty = true
}