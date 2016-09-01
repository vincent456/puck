/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph

import puck.util.LoggedEither._

import scalaz.std.list._
import ShowDG._

object Type {
  def findOverriddenIn
  ( g : DependencyGraph,
    absName : String, absSig : Type,
    candidates : List[TypedNode]) : Option[(TypedNode, List[TypedNode])] = {
    //println(s"searching for abstraction of $absName $absSig in $candidates")
    import puck.util.Collections.SelectList
    candidates.select{
      case (c, t) => c.name == absName && absSig.canOverride(g, t)
    }
  }

  def findOverridingIn
  ( g : DependencyGraph,
    implName : String, implSig : Type,
    candidates : List[TypedNode]) : Option[(TypedNode, List[TypedNode])] = {
    //println(s"searching for abstraction of $absName $absSig in $candidates")
    import puck.util.Collections.SelectList
    candidates.select {
      case (c, t) => c.name == implName && t.canOverride(g, implSig)
    }
  }


  type OnImplemNotFound =
    (DependencyGraph, ConcreteNode, List[TypedNode]) => LoggedTry[(DependencyGraph, List[TypedNode])]

  def errorOnImplemNotFound(className : String) : OnImplemNotFound = {
    (g, supMeth, _) =>
      LoggedError(s"$className has no implementation of ${(g, supMeth).shows}")
  }

  val ignoreOnImplemNotFound : OnImplemNotFound = {
    (g, _, cs) => LoggedSuccess((g,cs))
  }

  def findAndRegisterOverridedInList
  ( g : DependencyGraph,
    absMeths : List[TypedNode],
    candidates : List[TypedNode])
  ( onImplemNotFound : OnImplemNotFound ): LoggedTG =
    absMeths.foldLoggedEither((g, candidates) ){
      case ((g0, cs), (supMeth, supMethT)) =>
        supMeth.kind.kindType match {
          case InstanceValue =>
            findOverriddenIn(g0, supMeth.name, supMethT, cs) match {
              case Some(((subMeth, _), newCandidates)) =>
                LoggedSuccess((g0.addAbstraction(subMeth.id, AccessAbstraction(supMeth.id, SupertypeAbstraction)), newCandidates))
              case None => onImplemNotFound(g0, supMeth, candidates)
            }
          case TypeConstructor | StableValue => LoggedSuccess((g0,cs))
          case skt => LoggedError(s"findAndRegisterOverridedInList : ${(g, supMeth).shows} has an unexpected type kind ($skt)")
        }
    } map(_._1)


  def mainId(t : Type) : NodeId = t match {
    case NamedType(id) => id
    case ParameterizedType(id, _) => id
    case Covariant(tt) => mainId(tt)
    case Contravariant(tt) => mainId(tt)
    case _ => error()
  }
}

sealed abstract class Type {

  def makeClone() : Type
  def subtypeOf(graph : DependencyGraph,
                other : Type) : Boolean = this == other

  def ids : List[NodeId]

  def changeNamedType(oldUsed : NodeId, newUsed: NodeId) : Type

  def changeNamedTypeContravariant(oldUsed : NodeId, newUsed: NodeId) =
    changeNamedType(oldUsed, newUsed)

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

  def changeNamedType(oldUsed : NodeId, newUsed: NodeId) : NamedType =
    if(id == oldUsed) copy(newUsed)
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

  def changeNamedType(oldUsed : NodeId, newUsed: NodeId) : Tuple =
    copy(types.map(_.changeNamedType(oldUsed, newUsed)))

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

  override def changeNamedTypeContravariant(oldUsed : NodeId, newUsed: NodeId) =
    copy(input.changeNamedType(oldUsed, newUsed), output)

  override def subtypeOf(graph : DependencyGraph,
                         other : Type) : Boolean =
    super.subtypeOf(graph, other) ||
    ( other match{
      case Arrow(i : Type, o : Type) =>
        i.subtypeOf(graph, input) && output.subtypeOf(graph, o)
      case _ => false })


}

sealed abstract class VariantType extends Type {
  def t : Type
  protected val make : Type => VariantType

  def uses(id: NodeId): Boolean= t uses id
  def ids: List[NodeId] = t.ids
  def makeClone(): VariantType = make(t.makeClone())
  def changeNamedType(oldUsed: NodeId, newUsed: NodeId): VariantType =
    make(t.changeNamedType(oldUsed, newUsed))
}
case class Covariant(t : Type) extends VariantType {
  val make = Covariant apply _
}
case class Contravariant(t : Type) extends VariantType {
  val make = Contravariant apply _
}
//case class Invariant(t : Type) extends TypeParameter {
//  val make = Invariant _
//}
case class ParameterizedType(genType : NodeId, params : List[Type])
  extends Type {
  def makeClone(): Type = copy()

  def uses(id: NodeId): Boolean = genType == id || params.exists(_.ids contains id)

  def ids: List[NodeId] = genType :: params.flatMap(_.ids)

  def changeNamedType(oldUsed: NodeId, newUsed: NodeId): Type = {
    def f(id : NodeId) = if(id == oldUsed) newUsed else id
    copy(genType = f(genType), params = params map (_.changeNamedType(oldUsed, newUsed)))
  }
}