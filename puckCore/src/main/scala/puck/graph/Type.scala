package puck.graph

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph.constraints.SupertypeAbstraction
import puck.util.LoggedEither._

import scalaz.std.list._

object Type {
  def findOverridedIn
  ( g : DependencyGraph,
    absName : String, absSig : Type,
    candidates : List[ConcreteNode]) : Option[(ConcreteNode, List[ConcreteNode])] = {
    import puck.util.Collections.SelectList
    candidates.select( c => c.styp.nonEmpty &&
      c.name == absName &&
      c.styp.exists(absSig.canOverride(g, _)))
  }

  type OnImplemNotFound =
    (DependencyGraph, ConcreteNode, List[ConcreteNode]) => LoggedTry[(DependencyGraph, List[ConcreteNode])]

  def errorOnImplemNotFound(className : String) : OnImplemNotFound = {
    (g, supMeth, _) =>
      LoggedError(new PuckError(s"$className has no implementation of ${showDG[ConcreteNode](g).shows(supMeth)}"))
  }

  val ignoreOnImplemNotFound : OnImplemNotFound = {
    (g, _, cs) => g.toLoggedTG.map((_,cs))
  }

  def findAndRegisterOverridedInList
  ( g : DependencyGraph,
    absMeths : List[ConcreteNode],
    candidates : List[ConcreteNode])
  ( onImplemNotFound : OnImplemNotFound ): LoggedTG =
    absMeths.foldLoggedEither((g, candidates) ){
      case ((g0, cs), supMeth) =>
        (supMeth.styp, g.kindType(supMeth)) match {
          case (Some(mt), TypeMember) =>
            findOverridedIn(g0, supMeth.name, mt, cs) match {
              case Some((subMeth, newCandidates)) =>
                LoggedSuccess((g0.addAbstraction(subMeth.id, AccessAbstraction(supMeth.id, SupertypeAbstraction)), newCandidates))
              case None => onImplemNotFound(g0, supMeth, candidates)
            }
          case (Some(mt), TypeConstructor) => LoggedSuccess((g0,cs))
          case _ => LoggedError(new PuckError(s"${showDG[ConcreteNode](g).shows(supMeth)} has not a correct type"))
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

