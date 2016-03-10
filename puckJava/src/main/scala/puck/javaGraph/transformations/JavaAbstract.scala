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
package transformations

import nodeKind._
import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.rules.Abstract

object JavaAbstract extends Abstract {

  implicit class MyStringOps(val str: String) extends AnyVal {
    def upFirst =
      str.length match {
        case 0 => str
        case 1 => str.charAt(0).toUpper.toString
        case _ => str.charAt(0).toUpper + str.substring(1)
      }

  }

  override def abstractionName
  (g: DependencyGraph,
   impl: ConcreteNode,
   abskind: NodeKind,
   policy: AbstractionPolicy,
   sUsesAccessKind: Option[UsesAccessKind]
    ): String =
    (impl.kind, abskind, policy, sUsesAccessKind) match {
      case (Constructor, _, _, _) => "create"
      case (Field, Method, DelegationAbstraction, Some(Read)) => "get" + impl.name.upFirst
      case (Field, Method, DelegationAbstraction, Some(Write)) => "set" + impl.name.upFirst
      case (_, Method, SupertypeAbstraction, _)
           | (_, AbstractMethod, SupertypeAbstraction, _) => impl.name
      case _ => super.abstractionName(g, impl, abskind, policy, sUsesAccessKind)

    }

}