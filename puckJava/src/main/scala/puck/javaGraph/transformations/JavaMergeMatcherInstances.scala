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

package puck.javaGraph.transformations

import puck.graph.ConcreteNode
import puck.graph.transformations.{MergeMatcher, MergeMatcherInstances}
import puck.javaGraph.nodeKind.Field
/**
 * Created by Loïc Girault on 4/2/15.
 */
object JavaMergeMatcherInstances extends MergeMatcherInstances {

  def syntaxicMergeMatcher(n : ConcreteNode): MergeMatcher =
    n.kind match {
      case Field => new FieldMergeMatcher(n)
      case _ => semanticMergeMatcher(n)
    }

  def semanticMergeMatcher(n : ConcreteNode): MergeMatcher  =
    InterfaceMergeMatcher.mergeMatcher(n)

}
