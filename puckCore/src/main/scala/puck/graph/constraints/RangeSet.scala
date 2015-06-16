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

  val declare : String = ""

  def iterator : Iterator[Range] = setDef.iterator
  def +(n : Range) = new NamedRangeSet(id, setDef + n)
  def -(n : Range) = new NamedRangeSet(id, setDef - n)
  def literalCopy() = LiteralRangeSet(setDef)
}

class NamedRangeSetUnion(id0 : String,
                        override val setDef : RangeSetUnion)
  extends NamedRangeSet(id0, setDef){
  override val declare = "declareUnionSet"
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
    sets.foldLeft(s){case (s0, set0) =>  s0 ++ set0 }
    s.iterator
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