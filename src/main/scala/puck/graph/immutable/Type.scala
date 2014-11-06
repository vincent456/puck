package puck.graph.immutable

import AccessGraph.NodeId

/**
 * Created by lorilan on 28/10/14.
 */
abstract class Type[T <: Type[T]] {

  type NIdT = NodeId

  def copy() : T
  def subtypeOf(other : Type[_]) : Boolean = this == other

  def redirectUses(oldUsee : NIdT, newUsee: AGNode) : T

  def canOverride(other : Type[_]) : Boolean = this subtypeOf other
}

case class NamedType(node : NodeId, name : String)
  extends Type[ NamedType]{
  override def toString = name

  override def equals(other : Any) = other match {
    case that : NamedType => that.node == this.node
    case _ => false
  }

  def create(n : NodeId, name : String) = NamedType(n, name)

  def copy() = create(node, name)

  def redirectUses(oldUsee : NIdT, newUsee: AGNode) =
    if(node == oldUsee) create(newUsee.id, newUsee.name)
    else copy()

  override def subtypeOf(other : Type[_]) : Boolean = ??? /*super.subtypeOf(other) ||
    (other match {
      //TODO fix cast
      case NamedType(othern) => othern.asInstanceOf[NodeId[Kind]] isSuperTypeOf node
      case _ => false
    })*/

}

case class Tuple[T <: Type[T]](types: Seq[T])
  extends Type[Tuple[T]] {
  override def toString = types mkString ("(", ", ", ")")

  override def equals(other : Any) = other match {
    case Tuple(ts) => types.length == ts.length &&
      ((types, ts).zipped forall {
        case (s : Type[_], t: Type[_]) => s == t
      })
    case _ => false
  }

  def create(ts: Seq[T]) = Tuple[T](ts)
  def copy() = create(types)

  def redirectUses(oldUsee : NIdT, newUsee: AGNode) : Tuple[T] =
    create(types.map(_.redirectUses(oldUsee, newUsee)))

  override def subtypeOf(other : Type[_]) : Boolean = super.subtypeOf(other) ||
    (other match {
      case Tuple(ts) => types.length == ts.length &&
        ((types, ts).zipped forall {
          case (s : Type[_], t: Type[_]) => s.subtypeOf(t)
        })
      case _ => false
    })

  def length = types.length
}

case class Arrow[T <: Type[T],
                 S <: Type[S]](input : T, output : S)
  extends Type[Arrow[T, S]]{
  override def toString = input + " -> " + output

  override def equals(other : Any) : Boolean = other match {
    case Arrow(i : Type[_], o : Type[_]) => i == input  && output == o
    case _ => false
  }

  def create(i : T, o : S) = Arrow[T, S](i, o)
  def copy() = create(input.copy(), output.copy())

  def redirectUses(oldUsee : NIdT, newUsee: AGNode) : Arrow[T, S]=
    create(input.redirectUses(oldUsee, newUsee),
      output.redirectUses(oldUsee, newUsee))

  def redirectContravariantUses(oldUsee : NIdT, newUsee: AGNode) =
    create(input, output.redirectUses(oldUsee, newUsee))

  override def subtypeOf(other : Type[_]) : Boolean = ??? /*super.subtypeOf(other) ||
    ( other match{
      case Arrow(i : Type[Kind, _], o : Type[Kind, _]) => i.subtypeOf(input) && output.subtypeOf(o)
      case _ => false })*/

}

