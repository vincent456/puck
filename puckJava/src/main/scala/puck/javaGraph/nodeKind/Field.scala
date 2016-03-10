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

package puck.javaGraph
package nodeKind

import puck.graph.{InstanceValueDecl, StaticValueDecl, KindType, NodeKind}
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}


case object StaticField extends JavaNodeKind{
  override def isWritable = true

  def canContain(k : NodeKind) = k == Definition

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq()
    case DelegationAbstraction => Seq(StaticMethod)
  }
  override def abstractionPolicies = Seq(DelegationAbstraction)

  override def kindType: KindType = StaticValueDecl
}

case object Field extends JavaNodeKind {

  override def kindType: KindType = InstanceValueDecl
  override def isWritable = true

  def canContain(k : NodeKind) = k == Definition

  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> ()
  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
      case SupertypeAbstraction => Seq()
      case DelegationAbstraction => Seq(Method)
    }


  override def abstractionPolicies = Seq(DelegationAbstraction)
}


case object EnumConstant extends JavaNodeKind{

  def canContain(k : NodeKind) = false

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq()
    case DelegationAbstraction => Seq(StaticMethod)
  }
  override def abstractionPolicies = Seq(DelegationAbstraction)

  override def kindType: KindType = StaticValueDecl
}