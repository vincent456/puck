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

object RangeSet {
  def empty() = LiteralRangeSet()
}

object RangeBuilder {
 sealed abstract class Builder {
   def apply(n : NodeId) : Range
 }
 case object Scope extends Builder{
   def apply(n : NodeId) = puck.graph.constraints.Scope(n)
 }
 case object Element extends Builder{
   def apply(n : NodeId) = puck.graph.constraints.Element(n)
 }
}

sealed trait Range{
  val nid : NodeId
  //def productPrefix : String
  def contains_*(graph: DependencyGraph, other : NodeId) : Boolean

  def toElement : Range
}
case class Scope(nid : NodeId) extends Range {

  def contains_*(graph: DependencyGraph, other : NodeId) =
    graph.contains_*(nid, other)

  def toElement : Range = Element(nid)
}
case class Element(nid : NodeId) extends Range {
  def contains_*(graph: DependencyGraph, other : NodeId) =
    nid == other

  def toElement : Range = this
}

sealed trait RangeSet extends Iterable[Range] {
  type GraphT = DependencyGraph
  def +(n : Range) : RangeSet
  def -(n : Range) : RangeSet

  def rangeThatContains_*(graph : GraphT, elem: NodeId) =
    this find (_.contains_*(graph, elem))

  def hasRangeThatContains_*(graph : GraphT, elem: NodeId) =
    this exists (_.contains_*(graph, elem))

  def literalCopy() : LiteralRangeSet
}

case class RootedRangeSet(rs : RangeSet) extends RangeSet {
  def +(n : Range) : RangeSet = copy(rs + n)
  def -(n : Range) : RangeSet = copy(rs - n)

  def iterator : Iterator[Range] = rs.iterator map (_.toElement)

  def literalCopy() : LiteralRangeSet = LiteralRangeSet(this.iterator)
}

case class NamedRangeSet private[constraints]
 (id : String,
  setDef : RangeSetDef) extends RangeSet{

  override def toString() = id

  val declare_pre : String = ""
  val declare_post : String = ""

  def iterator : Iterator[Range] = setDef.iterator
  def +(n : Range) = new NamedRangeSet(id, setDef + n)
  def -(n : Range) = new NamedRangeSet(id, setDef - n)
  def literalCopy() = LiteralRangeSet(setDef)
}

class NamedRangeSetUnion(id0 : String,
                        override val setDef : RangeSetUnion)
  extends NamedRangeSet(id0, setDef){
  override val declare_pre = "union("
  override val declare_post = ")"
}


sealed trait RangeSetDef extends RangeSet{
  def +(n : Range) : RangeSetDef
  def -(n : Range) : RangeSetDef
}

case class RangeSetUnion private[constraints]
( sets : Seq[RangeSet],
  set : LiteralRangeSet) extends RangeSetDef {

  def iterator : Iterator[Range] = {
    val s = Seq[Range]() ++ set
    val s1 = sets.foldLeft(s){case (s0, set0) =>  s0 ++ set0 }
    s1.iterator
  }

  def +(n : Range) = new RangeSetUnion(sets, set + n)

  def -(n : Range) = throw new DGError("Do not know how to remove a node from a nodeSet union")

  def literalCopy() = LiteralRangeSet(this.iterator)
}

case class RangeSetDiff private[constraints]
( plus : RangeSet,
  minus : RangeSet) extends RangeSetDef{

  def iterator : Iterator[Range] =
    (Set[Range]() ++ plus -- minus).iterator

  def +(n: Range) = copy(plus = plus + n)
  def -(n: Range) = copy(minus = minus + n)
  def literalCopy() = LiteralRangeSet(this.iterator)
}

case class LiteralRangeSet private[constraints]
( content : Set[Range]) extends RangeSetDef{

  def iterator : Iterator[Range] = content.iterator
  def +(n : Range) = copy(content = content + n)
  def ++ (ns : RangeSet) : LiteralRangeSet = copy(content = content ++ ns)
  def -(n : Range) = copy(content = content - n)

  def literalCopy() = this
}

object LiteralRangeSet{
  val empty = new LiteralRangeSet(Set())
  def apply() = empty
  def apply(n : Range) = new LiteralRangeSet(Set(n))
  def apply(ns : TraversableOnce[Range]) = {
    new LiteralRangeSet(Set() ++ ns)
  }
}