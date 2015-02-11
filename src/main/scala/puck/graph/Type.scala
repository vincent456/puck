package puck.graph

/**
 * Created by lorilan on 28/10/14.
 */
abstract class Type[T <: Type[T]] {

  type NIdT = NodeId

  def copy() : T
  def subtypeOf(other : Type[_]) : Boolean = this == other

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) : T

  def canOverride(other : Type[_]) : Boolean = this subtypeOf other
}

case class NamedType(id : NodeId)
  extends Type[NamedType]{

  override def equals(other : Any) = other match {
    case that : NamedType => that.id == this.id
    case _ => false
  }

  def create(n : NodeId) = NamedType(n)

  def copy() = create(id)

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) =
    if(id == oldUsee) create(newUsee.id)
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

  override def equals(other : Any) = other match {
    case Tuple(ts) => types.length == ts.length &&
      ((types, ts).zipped forall {
        case (s : Type[_], t: Type[_]) => s == t
      })
    case _ => false
  }

  def create(ts: Seq[T]) = Tuple[T](ts)
  def copy() = create(types)

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) : Tuple[T] =
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

  override def equals(other : Any) : Boolean = other match {
    case Arrow(i : Type[_], o : Type[_]) => i == input  && output == o
    case _ => false
  }

  def create(i : T, o : S) = Arrow[T, S](i, o)
  def copy() = create(input.copy(), output.copy())

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) : Arrow[T, S]=
    create(input.redirectUses(oldUsee, newUsee),
      output.redirectUses(oldUsee, newUsee))

  def redirectContravariantUses(oldUsee : NIdT, newUsee: DGNode) =
    create(input.redirectUses(oldUsee, newUsee), output)

  override def subtypeOf(other : Type[_]) : Boolean = ??? /*super.subtypeOf(other) ||
    ( other match{
      case Arrow(i : Type[Kind, _], o : Type[Kind, _]) => i.subtypeOf(input) && output.subtypeOf(o)
      case _ => false })*/

}

