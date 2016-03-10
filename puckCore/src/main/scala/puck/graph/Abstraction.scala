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

import puck.graph.constraints.{DelegationAbstraction, AbstractionPolicy}

import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

sealed abstract class Abstraction {
  def policy: AbstractionPolicy
  def nodes : List[NodeId]
  def kind(g : DependencyGraph) : NodeKind
  def kindType(g : DependencyGraph) : KindType
  def containerIn(g : DependencyGraph) :  Option[NodeId]
}

case class AccessAbstraction
( nodeId : NodeId,
  policy: AbstractionPolicy
  ) extends Abstraction {
  def nodes : List[NodeId] = List(nodeId)
  def kindType(g : DependencyGraph) : KindType = g.kindType(nodeId)
  def kind(g : DependencyGraph) : NodeKind = g.getNode(nodeId).kind
  def containerIn(g : DependencyGraph) : Option[NodeId] = g.container(nodeId)
}

case class ReadWriteAbstraction
( readAbsNode : Option[NodeId],
  writeAbsNode : Option[NodeId]
  ) extends Abstraction {
  def policy = DelegationAbstraction
  def nodes : List[NodeId] =
    List(readAbsNode, writeAbsNode).filter(_.nonEmpty).sequence.getOrElse(List())


  def bothSameValueOrElse[T](f: NodeId => T, default: => T) : T =
    (readAbsNode, writeAbsNode) match {
      case (None,None) => default
      case (Some(nodeId), None) =>  f(nodeId)
      case (None, Some(nodeId)) =>  f(nodeId)
      case (Some(nodeId0), Some(nodeId)) =>
        val v = f(nodeId)
        assert(f(nodeId0) == v)
        v
    }


  def kindType(g : DependencyGraph) : KindType =
    bothSameValueOrElse(g.kindType, UnknownKindType)

  def kind(g : DependencyGraph) : NodeKind =
    bothSameValueOrElse(g.getNode(_).kind, error("should have same kind"))


  def containerIn(g : DependencyGraph) =
    bothSameValueOrElse(g.container, None)

}
