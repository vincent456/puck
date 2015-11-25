package puck.graph

import puck.graph.constraints.SupertypeAbstraction
import puck.util.LoggedEither._

import scalaz.std.list._
import ShowDG._

object Type {
  def findOverridedIn
  ( g : DependencyGraph,
    absName : String, absSig : Type,
    candidates : List[TypedNode]) : Option[(TypedNode, List[TypedNode])] = {
    //println(s"searching for abstraction of $absName $absSig in $candidates")
    import puck.util.Collections.SelectList
    candidates.select{
      case (c, t) => c.name == absName && absSig.canOverride(g, t)
    }
  }

  type OnImplemNotFound =
    (DependencyGraph, ConcreteNode, List[TypedNode]) => LoggedTry[(DependencyGraph, List[TypedNode])]

  def errorOnImplemNotFound(className : String) : OnImplemNotFound = {
    (g, supMeth, _) =>
      LoggedError(s"$className has no implementation of ${(g, supMeth).shows}")
  }

  val ignoreOnImplemNotFound : OnImplemNotFound = {
    (g, _, cs) => g.toLoggedTG.map((_,cs))
  }

  def findAndRegisterOverridedInList
  ( g : DependencyGraph,
    absMeths : List[TypedNode],
    candidates : List[TypedNode])
  ( onImplemNotFound : OnImplemNotFound ): LoggedTG =
    absMeths.foldLoggedEither((g, candidates) ){
      case ((g0, cs), (supMeth, supMethT)) =>
        supMeth.kind.kindType match {
          case InstanceValueDecl =>
            findOverridedIn(g0, supMeth.name, supMethT, cs) match {
              case Some(((subMeth, _), newCandidates)) =>
                LoggedSuccess((g0.addAbstraction(subMeth.id, AccessAbstraction(supMeth.id, SupertypeAbstraction)), newCandidates))
              case None => onImplemNotFound(g0, supMeth, candidates)
            }
          case TypeConstructor | StaticValueDecl => LoggedSuccess((g0,cs))
          case skt => LoggedError(s"findAndRegisterOverridedInList : ${(g, supMeth).shows} has an unexpected type kind ($skt)")
        }
    } map(_._1)


}

sealed abstract class Type {

  def makeClone() : Type
  def subtypeOf(graph : DependencyGraph,
                other : Type) : Boolean = this == other

  def ids : List[NodeId]

  def changeNamedType(oldUsee : NodeId, newUsee: NodeId) : Type

  def changeNamedTypeContravariant(oldUsee : NodeId, newUsee: NodeId) =
    changeNamedType(oldUsee, newUsee)

  def canOverride(graph : DependencyGraph,
                  other : Type) : Boolean = this.subtypeOf(graph, other)

  def uses( id : NodeId) : Boolean
}

case class NamedType(id : NodeId)
  extends Type{

  override def equals(other : Any) = other match {
    case that : NamedType => that.id == this.id
    case _ => false
  }

  def ids = List(id)

  def uses( id : NodeId) : Boolean = id == this.id

  override def makeClone() = copy(id)

  def changeNamedType(oldUsee : NodeId, newUsee: NodeId) : NamedType =
    if(id == oldUsee) copy(newUsee)
    else makeClone()

  override def subtypeOf(graph : DependencyGraph,
                         other : Type) : Boolean =
    super.subtypeOf(graph, other) ||
    (other match {
      case NamedType(otherId) =>
        graph.isa_*(this.id, otherId)
      case _ => false
    })

}

case class Tuple(types: List[Type] = List())
  extends Type {

  override def equals(other : Any) = other match {
    case Tuple(ts) => types.length == ts.length &&
      ((types, ts).zipped forall {
        case (s : Type, t: Type) => s == t
      })
    case _ => false
  }

  def uses( id : NodeId) : Boolean = types.exists(_.uses(id))

  def ids = types.foldLeft(List[NodeId]()){(acc, t) => t.ids ::: acc }

  override def makeClone() : Tuple = copy(types)

  def changeNamedType(oldUsee : NodeId, newUsee: NodeId) : Tuple =
    copy(types.map(_.changeNamedType(oldUsee, newUsee)))

  override def subtypeOf(graph : DependencyGraph,
                         other : Type) : Boolean =
    super.subtypeOf(graph, other) ||
    (other match {
      case Tuple(ts) => types.length == ts.length &&
        ((types, ts).zipped forall {(s , t) => s.subtypeOf(graph, t)})
      case _ => false
    })

  def length : Int = types.length
}

case class Arrow(input : Type, output : Type)
  extends Type {

  def ids = output.ids ::: input.ids

  def uses( id : NodeId) : Boolean = input.uses(id) || output.uses(id)

  override def equals(other : Any) : Boolean = other match {
    case Arrow(i : Type, o : Type) => i == input  && output == o
    case _ => false
  }

  def removeFirstArgOfType(n : Type) : Arrow = ???
  def prependParameter(t : Type) : Arrow =
    Arrow(t, makeClone())

  def uncurry : Arrow = {
      val newOutput = output match {
        case a : Arrow => a.uncurry
        case _ => output
      }

    newOutput match {
      case Arrow(Tuple(is), o) =>
        Arrow(Tuple(input :: is), o)
      case Arrow(i, o) => Arrow(Tuple(List(input, i)), o)
      case _ => this
    }
  }



  override def makeClone() : Arrow =
    copy(input.makeClone(), output.makeClone())

  def changeNamedType(oldUsee : NodeId, newUsee: NodeId) : Arrow =
    copy(input.changeNamedType(oldUsee, newUsee),
      output.changeNamedType(oldUsee, newUsee))

  override def changeNamedTypeContravariant(oldUsee : NodeId, newUsee: NodeId) =
    copy(input.changeNamedType(oldUsee, newUsee), output)

  override def subtypeOf(graph : DependencyGraph,
                         other : Type) : Boolean =
    super.subtypeOf(graph, other) ||
    ( other match{
      case Arrow(i : Type, o : Type) =>
        i.subtypeOf(graph, input) && output.subtypeOf(graph, o)
      case _ => false })


}

