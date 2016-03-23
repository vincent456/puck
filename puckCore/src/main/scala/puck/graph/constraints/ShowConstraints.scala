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
import scalaz.Cord

trait ShowConstraints {
  type CordBuilder[A] = (DependencyGraph, A) => Cord

  implicit def rangeCord : CordBuilder[Range] = { (dg, r) =>
    val prefix = r match { case Scope(id) => "'" case Element(id) => "r:'" }
    Cord(prefix, dg.fullName(r.nid), "'")
  }

  def namedRangeSetDefCord : CordBuilder[NamedRangeSet] = (dg, nrs) =>
      Cord(nrs.id, " = ", nrs.declare_pre, rangeSetCord(dg, nrs.setDef), nrs.declare_post)



  implicit def rangeSetCord : CordBuilder[RangeSet] = { (dg, rs) =>
    rs match {
      case RootedRangeSet(s) => "r:" +: rangeSetCord(dg, s)
      case NamedRangeSet(id, setDef) => id
      case RangeSetUnion(sets, set) =>
        Cord("[", Cord.mkCord(",\n", sets.map(rangeSetCord(dg,_)).toSeq:_*), ",\n",
          Cord.mkCord(",\n", set.map(rangeCord(dg,_)).toSeq:_*), "]")
      case RangeSetDiff(plus, minus) =>
        Cord(rangeSetCord(dg, plus), "\\", rangeSetCord(dg, minus))
      case LiteralRangeSet(content) =>
        Cord("[", Cord.mkCord(",\n", content.map(rangeCord(dg,_)).toSeq:_*), "]")
    }
  }
  def friendConstraintCord : CordBuilder[Constraint] = (dg, c) =>
    Cord(rangeSetCord(dg, c.friends), s" $friendOf ",  rangeSetCord(dg, c.owners))


  implicit def constraintCord : CordBuilder[Constraint] = {(dg, c) =>

    val c0 = Cord(hide, " ", rangeSetCord(dg, c.owners))

    val c1 =
      if(c.facades.isEmpty) c0
      else Cord(c0, s" $except ", rangeSetCord(dg, c.facades))

    val c2 =
      if(c.interlopers.isEmpty) c1
      else Cord(c1, s" $from ", rangeSetCord(dg, c.interlopers))

    if(c.friends.isEmpty) c2
    else Cord(c2, s" $butNotFrom ", rangeSetCord(dg, c.friends))

  }

  implicit def constraintSetCord :  CordBuilder[ConstraintSet] = { (dg, cs) =>
    Cord.mkCord("\n", cs.content.map(constraintCord(dg, _)).toSeq:_*)
  }
}

//object ShowConstraints {
//  import ShowDG.CordBuilder
//
//  implicit def rangeCord : CordBuilder[Range] = { (dg, r) =>
//    val prefix = r match { case Scope(id) => "'" case Element(id) => "e:'" }
//    Cord(prefix, dg.fullName(r.nid), "'")
//  }
//
//  def namedRangeSetDefCord : CordBuilder[NamedRangeSet] = (dg, nrs) =>
//    Cord(nrs.declare, "(" + nrs.id + ", ", rangeSetCord(dg, nrs.setDef),").")
//
//  implicit def rangeSetCord : CordBuilder[RangeSet] = { (dg, rs) =>
//    rs match {
//      case NamedRangeSet(id, setDef) => id
//      case RangeSetUnion(sets, set) =>
//        Cord("[", Cord.mkCord(",\n", sets.map(rangeSetCord(dg,_)).toSeq:_*), ",\n",
//            Cord.mkCord(",\n", set.map(rangeCord(dg,_)).toSeq:_*), "]")
//      case RangeSetDiff(plus, minus) =>
//        Cord(rangeSetCord(dg, plus), "\\", rangeSetCord(dg, minus))
//      case LiteralRangeSet(content) =>
//        Cord("[", Cord.mkCord(",\n", content.map(rangeCord(dg,_)).toSeq:_*), "]")
//    }
//  }
//
//  implicit def constraintCord : CordBuilder[Constraint] = {(dg, c) =>
//
//    def twoArgsFormat(prefix : String, set : RangeSet) =
//      Cord(prefix, "(", rangeSetCord(dg, c.owners), ", ", rangeSetCord(dg, set), ").")
//
//    (c.facades.isEmpty, c.interlopers.isEmpty, c.friends.isEmpty) match {
//      case (true, false, true) =>
//        if(dg.isRoot(c.interlopers.head.nid))
//          Cord("hide(", rangeSetCord(dg, c.owners), ").")
//        else
//          twoArgsFormat("hideFrom", c.interlopers)
//      case (true, false, false)
//        if dg.isRoot(c.interlopers.head.nid) => twoArgsFormat("hideButFrom", c.friends)
//      case (false, false, true)
//        if dg.isRoot(c.interlopers.head.nid) => twoArgsFormat("hideBut", c.facades)
//      case (_, _, _) => Cord("hide(", rangeSetCord(dg, c.owners), ",\n",
//        rangeSetCord(dg, c.facades), ",\n",
//        rangeSetCord(dg, c.interlopers),  ",\n",
//        rangeSetCord(dg, c.friends), ").")
//    }
//  }
//
//  implicit def constraintSetCord :  CordBuilder[ConstraintSet] = { (dg, cs) =>
//    Cord.mkCord("\n", cs.content.map(constraintCord(dg, _)).toSeq:_*)
//  }
//}
