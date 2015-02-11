package puck.graph

/**
 * Created by lorilan on 1/8/15.
 */
trait TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: DGNode) : TypeHolder
  def redirectContravariantUses(oldUsee : NodeId, newUsee: DGNode) : TypeHolder
  def getTypeNodeIds : List[NodeId]
  def isEmpty = false

}
case object NoType extends TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: DGNode) = this
  def redirectContravariantUses(oldUsee : NodeId, newUsee: DGNode) = this

  def getTypeNodeIds : List[NodeId] = List()

  override def isEmpty = true
}