package puck.graph

/**
 * Created by lorilan on 28/10/14.
 */
abstract class Type {

  type NIdT = NodeId

  def copy() : Type
  def subtypeOf(graph : DependencyGraph,
                other : Type) : Boolean = this == other

  def ids : List[NodeId]

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) : Type

  def canOverride(graph : DependencyGraph,
                  other : Type) : Boolean = this.subtypeOf(graph, other)
}

case class NamedType(id : NodeId)
  extends Type{

  override def equals(other : Any) = other match {
    case that : NamedType => that.id == this.id
    case _ => false
  }

  def ids = List(id)
  def create(n : NodeId) = NamedType(n)

  def copy() = create(id)

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) : NamedType =
    if(id == oldUsee) create(newUsee.id)
    else copy()

  override def subtypeOf(graph : DependencyGraph,
                         other : Type) : Boolean =
    super.subtypeOf(graph, other) ||
    (other match {
      case NamedType(otherId) =>
        graph.isSuperTypeOf(otherId, id)
      case _ => false
    })

}

case class Tuple(types: List[Type])
  extends Type {

  override def equals(other : Any) = other match {
    case Tuple(ts) => types.length == ts.length &&
      ((types, ts).zipped forall {
        case (s : Type, t: Type) => s == t
      })
    case _ => false
  }

  def ids = types.foldLeft(List[NodeId]()){(acc, t) => t.ids ::: acc }

  def create(ts: List[Type]) = Tuple(ts)
  def copy() = create(types)

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) : Tuple =
    create(types.map(_.redirectUses(oldUsee, newUsee)))

  override def subtypeOf(graph : DependencyGraph,
                         other : Type) : Boolean =
    super.subtypeOf(graph, other) ||
    (other match {
      case Tuple(ts) => types.length == ts.length &&
        ((types, ts).zipped forall {(s , t) => s.subtypeOf(graph, t)})
      case _ => false
    })

  def length = types.length
}

case class Arrow(input : Type, output : Type)
  extends Type {

  def ids = output.ids ::: input.ids

  override def equals(other : Any) : Boolean = other match {
    case Arrow(i : Type, o : Type) => i == input  && output == o
    case _ => false
  }

  def create(i : Type, o : Type) = Arrow(i, o)
  def copy() = create(input.copy(), output.copy())

  def redirectUses(oldUsee : NIdT, newUsee: DGNode) : Arrow =
    create(input.redirectUses(oldUsee, newUsee),
      output.redirectUses(oldUsee, newUsee))

  def redirectContravariantUses(oldUsee : NIdT, newUsee: DGNode) =
    create(input.redirectUses(oldUsee, newUsee), output)

  override def subtypeOf(graph : DependencyGraph,
                         other : Type) : Boolean =
    super.subtypeOf(graph, other) ||
    ( other match{
      case Arrow(i : Type, o : Type) =>
        i.subtypeOf(graph, input) && output.subtypeOf(graph, o)
      case _ => false })

}

