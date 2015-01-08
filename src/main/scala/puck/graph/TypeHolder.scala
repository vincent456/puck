package puck.graph

/**
 * Created by lorilan on 1/8/15.
 */
trait TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: AGNode) : TypeHolder
  def redirectContravariantUses(oldUsee : NodeId, newUsee: AGNode) : TypeHolder
  def mkString(graph : AccessGraph) : String
  def isEmpty = false

}
case object NoType extends TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: AGNode) = this
  def redirectContravariantUses(oldUsee : NodeId, newUsee: AGNode) = this
  def mkString(graph : AccessGraph) : String = ""
  override def isEmpty = true
}