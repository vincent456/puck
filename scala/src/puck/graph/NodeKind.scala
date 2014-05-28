package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */

abstract class NodeKind {
  def abstractKinds : List[NodeKind]
}
case class AGRoot private[graph]() extends NodeKind{
  override def abstractKinds = throw new AGError("Root node cannot be abstracted")

}
case class VanillaKind private[graph]() extends NodeKind{
  override val abstractKinds = List(new VanillaKind())
}

trait HasType[T<:Type] {
  var `type` : T = _
}