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
import Keywords._

trait ShowConstraints {
  type DGStringBuilder[A] = (DependencyGraph, A) => String

  implicit def stringOfRange : DGStringBuilder[Range] = { (dg, r) =>
    val prefix = r match { case Scope(id) => "'" case Element(id) => "r:'" }
      prefix + dg.fullName(r.nid) + "'"
  }

  def stringOfNamedRangeSetDef : DGStringBuilder[NamedRangeSet] = (dg, nrs) =>
      nrs.id + " = " + nrs.declare_pre + stringOfRangeSet(dg, nrs.setDef) + nrs.declare_post



  implicit def stringOfRangeSet : DGStringBuilder[RangeSet] = { (dg, rs) =>
    rs match {
      case RootedRangeSet(s) => "r:" + stringOfRangeSet(dg, s)
      case NamedRangeSet(id, setDef) => id
      case RangeSetUnion(sets, set) =>
        sets.map(stringOfRangeSet(dg,_)).mkString ("[",",\n","\n") +
          set.map(stringOfRange(dg,_)).mkString("", "\n", "]")
      case RangeSetDiff(plus, minus) =>
        stringOfRangeSet(dg, plus) + "\\" + stringOfRangeSet(dg, minus)
      case LiteralRangeSet(content) =>
        content.map(stringOfRange(dg,_)).mkString ("[",",\n","]")
    }
  }
  def stringOfFriendConstraint : DGStringBuilder[Constraint] = (dg, c) =>
    s"${stringOfRangeSet(dg, c.friends)} $friendOf ${stringOfRangeSet(dg, c.owners)}"


  implicit def stringOfConstraint : DGStringBuilder[Constraint] = { (dg, c) =>

    val c0 = s"$hide ${stringOfRangeSet(dg, c.owners)}"

    val c1 =
      if(c.facades.isEmpty) c0
      else s"$c0 $except ${stringOfRangeSet(dg, c.facades)}"

    val c2 =
      if(c.interlopers.isEmpty) c1
      else s"$c1 $from ${stringOfRangeSet(dg, c.interlopers)}"

    if(c.friends.isEmpty) c2
    else s"$c2 $butNotFrom ${stringOfRangeSet(dg, c.friends)}"

  }

  implicit def stringOfConstraintSet :  DGStringBuilder[ConstraintSet] = { (dg, cs) =>
     cs.content.map(stringOfConstraint(dg, _)) mkString "\n"
  }
}
