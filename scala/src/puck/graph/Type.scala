package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */
abstract class Type
case class NamedType(val name: String, val id: Int)extends Type{

  override def toString = name
}
case class Tuple(val types:Type*) extends Type{
  override def toString = types mkString ("(", ", ", ")")
}

case class Arrow(val input:Type, val output:Type) extends Type{
  override def toString = input + " -> " + output
}
