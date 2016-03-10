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
package constraints

/**
 * Created by Loïc Girault on 11/06/14.
 */

object ConstraintSet{
  def empty = new ConstraintSet(Seq())
}
class ConstraintSet
(private[constraints] val content : Seq[Constraint])
  extends Iterable[Constraint]{

  def this() = this(Seq[Constraint]())

  // /!\ uses eq and not ==
  def replaceEq(ct : Constraint, newCt : Constraint) : ConstraintSet =
    new ConstraintSet(content.map {ct0 => if(ct0 eq ct) newCt else ct0})

  def replaceEq(cts : Seq[(Constraint, Constraint)]) : ConstraintSet =
    cts.foldLeft(this){
      case (m, (oldCt, newCt)) => m.replaceEq(oldCt, newCt)
    }

  def iterator = content.iterator

  def + (ct : Constraint) : ConstraintSet = new ConstraintSet( ct +: content )
  //def - (ct : Constraint) : ConstraintSet[Kind, Constraint] = new ConstraintSet( content - ct )

  def hasFriendRangeThatContains_*(graph : DependencyGraph, n : NodeId)=
    content.exists( _.friends.hasRangeThatContains_*(graph, n))

}