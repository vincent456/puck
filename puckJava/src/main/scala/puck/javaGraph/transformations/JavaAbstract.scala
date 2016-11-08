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
import puck.graph.transformations.rules.Abstract
import puck.graph.ShowDG._
import puck.util.LoggedEither._

import scalaz.std.set._

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

  override def createAbsNodeAndUse
  ( g : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy
    ) : (Abstraction, DependencyGraph) = {
    val (abs, g2) = super.createAbsNodeAndUse(g,impl,abskind,policy)
    val g3 = impl.kind match {
      case Field =>
        val ReadWriteAbstraction(Some(get), Some(set)) = abs
        val clazz = g.container_!(impl.id)

        val setDef = g2.definitionOf_!(set)
        val getDef = g2.definitionOf_!(get)
        val selfUse = (clazz, clazz)
        g2.addEdge(Contains(clazz, get))
          .addEdge(Contains(clazz, set))
          .addUses(clazz, clazz)
          // we define a setter that returns the new value to mimic the affectation value
          // hence the value is also read
          .addBinding(selfUse, (getDef, impl.id))
          .addAccessKind((selfUse, (getDef, impl.id)), Read)
          .addBinding(selfUse, (setDef, impl.id))
          .addAccessKind((selfUse, (setDef, impl.id)), RW)
      case _ : MethodKind =>
        //excludingTypeUse means exclude uses computed from type
        //when a method trow exception types, it uses these outside of type uses
        val AccessAbstraction(absId, _) = abs
        g.usedByExcludingTypeUse(impl.id).foldLeft(g2) {
          case (g0, thrownTypeId) => g0.addUses(absId, thrownTypeId)
        }
      case _ => g2
    }
    (abs, g3)
  }

  override def insertInTypeHierarchy
  ( g : DependencyGraph,
    subType : ConcreteNode,
    newSuperType : ConcreteNode
    ) : LoggedTG = {
    val objectId = DependencyGraph.findElementByName(g, "java.lang.Object").get.id
    s"insertInTypeHierarchy(g, ${(g, subType).shows}, ${(g, newSuperType).shows})" <++: {
      g.directSuperTypesId(subType.id).foldLoggedEither(g) {
        (g0, oldSuperTypedId) =>

          lazy val rmlog = s"removeIsa(${(g0, subType).shows}, ${(g0, oldSuperTypedId).shows})"
          lazy val addlog = s"addIsa(${(g0, newSuperType.id).shows}, ${(g0, oldSuperTypedId).shows})"
          lazy val g1 = g0.removeIsa(NamedType(subType.id), NamedType(oldSuperTypedId))
          def add(g : DependencyGraph) = g.addIsa(NamedType(newSuperType.id), NamedType(oldSuperTypedId))

          (oldSuperTypedId, newSuperType.kind) match {
            case (`objectId`, Interface) => LoggedSuccess(g0)
            case (`objectId`, _) => LoggedSuccess(rmlog, g1)
            case _ => LoggedSuccess(rmlog +"\n"+addlog, add(g1))
          }
      }
    }
  }

}