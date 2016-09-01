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

/**
  * Created by Loïc Girault on 15/04/16.
  */

object TypeConstraint {

  def comply(g : DependencyGraph, tuc : TypeConstraint, n : NodeId) : Boolean = tuc match {
    case Eq(other) => g.typ(n) == g.typ(other)
    case Sup(other) =>  g.typ(n).subtypeOf(g, g.typ(other))

    case Sub(other) => g.typ(other).subtypeOf(g, g.typ(n))

  }

  implicit class NodeIdOps(val gt : (DependencyGraph,NodeId)) extends AnyVal {
    def g = gt._1
    def t = gt._2
    def complyWith(tuc : TypeConstraint) : Boolean = comply(g, tuc ,t)
  }
}

sealed abstract class TypeConstraint {
  def typedNode : NodeId
}
case class Sup(typedNode : NodeId) extends TypeConstraint
case class Sub(typedNode : NodeId) extends TypeConstraint
case class Eq(typedNode : NodeId) extends TypeConstraint
// constraint against the value of type parameter of another node
sealed abstract class ParTypeConstraint extends TypeConstraint {
  def typeArgIndex : Int
}
case class ParSup(typedNode : NodeId, typeArgIndex : Int) extends ParTypeConstraint
case class ParSub(typedNode : NodeIdP, typeArgIndex : Int) extends ParTypeConstraint
case class ParEq(typedNode : NodeId, typeArgIndex : Int) extends ParTypeConstraint

