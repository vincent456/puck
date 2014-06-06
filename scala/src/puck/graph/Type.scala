package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */
abstract class Type {
  def subtypeOf(other : Type) : Boolean = this == other
}

case class NamedType(n : AGNode) extends Type{
  override def toString = n.name
  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    (other match {
      case NamedType(othern) => othern isSuperTypeOf n
      case _ => false
    })
}

case class Tuple(types: List[Type]) extends Type {
  override def toString = types mkString ("(", ", ", ")")

  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    (other match {
      case Tuple(ts) => (types, ts).zipped forall( _.subtypeOf(_))
      case _ => false
    })

}

case class Arrow(input:Type, output:Type) extends Type{
  override def toString = input + " -> " + output

  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    ( other match{
      case Arrow( i, o) => i.subtypeOf(input) && output.subtypeOf(o)
      case _ => false })

  def canOverride(other : Arrow) : Boolean = this subtypeOf other
}
