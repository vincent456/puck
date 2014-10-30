package puck.graph.immutable

import AccessGraph.NodeId

/**
 * Created by lorilan on 28/10/14.
 */
abstract class Type[Kind <: NodeKind[Kind], T <: Type[Kind, T]] {
  def copy() : T
  def subtypeOf(other : Type[Kind, _]) : Boolean = this == other

  trait Replacer{
    def replacedBy(newUsee: AGNode[Kind]) : T
  }
  def copyWith (oldUsee : NodeId[Kind]) : Replacer

  def canOverride(other : Type[Kind, _]) : Boolean = this subtypeOf other
}

case class NamedType[Kind <: NodeKind[Kind]](node : NodeId[Kind],
                                             name : String)
  extends Type[Kind, NamedType[Kind]]{
  override def toString = name

  override def equals(other : Any) = other match {
    case that : NamedType[Kind] => that.node == this.node
    case _ => false
  }

  def create(n : NodeId[Kind], name : String) = NamedType[Kind](n, name)

  def copy() = create(node, name)

  protected class NTReplacer (oldUsee : NodeId[Kind]) extends Replacer {
    def replacedBy(newUsee: AGNode[Kind]) =
      if(node == oldUsee) create(newUsee.id, newUsee.name)
      else copy()
  }
  def copyWith (oldUsee : NodeId[Kind]) = new NTReplacer(oldUsee)

  override def subtypeOf(other : Type[Kind, _]) : Boolean = ??? /*super.subtypeOf(other) ||
    (other match {
      //TODO fix cast
      case NamedType(othern) => othern.asInstanceOf[NodeId[Kind]] isSuperTypeOf node
      case _ => false
    })*/

}

case class Tuple[Kind <: NodeKind[Kind], T <: Type[Kind, T]](types: Seq[T])
  extends Type[Kind, Tuple[Kind, T]] {
  override def toString = types mkString ("(", ", ", ")")

  override def equals(other : Any) = other match {
    case Tuple(ts) => types.length == ts.length &&
      ((types, ts).zipped forall {
        case (s : Type[Kind, _], t: Type[Kind, _]) => s == t
      })
    case _ => false
  }

  def create(ts: Seq[T]) = Tuple[Kind, T](ts)
  def copy() = create(types)

  protected class TupleReplacer (oldUsee : NodeId[Kind]) extends Replacer{
    def replacedBy(newUsee: AGNode[Kind]) =
      create(types.map(_.copyWith(oldUsee).replacedBy(newUsee)))
  }

  def copyWith (oldUsee : NodeId[Kind]) = new TupleReplacer(oldUsee)


  override def subtypeOf(other : Type[Kind, _]) : Boolean = super.subtypeOf(other) ||
    (other match {
      case Tuple(ts) => types.length == ts.length &&
        ((types, ts).zipped forall {
          case (s : Type[Kind, _], t: Type[Kind, _]) => s.subtypeOf(t)
        })
      case _ => false
    })

  def length = types.length
}

case class Arrow[Kind <: NodeKind[Kind],
T <: Type[Kind, T],
S <: Type[Kind, S]](input : T, output : S)
  extends Type[Kind, Arrow[Kind, T, S]]{
  override def toString = input + " -> " + output

  override def equals(other : Any) : Boolean = other match {
    case Arrow(i : Type[Kind, _], o : Type[Kind, _]) => i == input  && output == o
    case _ => false
  }

  def create(i : T, o : S) = Arrow[Kind, T, S](i, o)
  def copy() = create(input.copy(), output.copy())

  protected class ArrowReplacer (oldUsee : NodeId[Kind]) extends Replacer{
    def replacedBy(newUsee: AGNode[Kind]) =
      create(input.copyWith(oldUsee).replacedBy(newUsee),
        output.copyWith(oldUsee).replacedBy(newUsee))


  }
  def copyWith (oldUsee : NodeId[Kind]) = new ArrowReplacer(oldUsee)

  override def subtypeOf(other : Type[Kind, _]) : Boolean = ??? /*super.subtypeOf(other) ||
    ( other match{
      case Arrow(i : Type[Kind, _], o : Type[Kind, _]) => i.subtypeOf(input) && output.subtypeOf(o)
      case _ => false })*/

}

