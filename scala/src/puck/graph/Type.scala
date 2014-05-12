package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */
abstract class Type
case class NamedType(name: String, id: Int)extends Type{
  override def toString = name
}
case class Tuple(types: List[Type]) extends Type{
  override def toString = types mkString ("(", ", ", ")")
}

case class Arrow(input:Type, output:Type) extends Type{
  override def toString = input + " -> " + output
}
