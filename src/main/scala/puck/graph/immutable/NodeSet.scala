package puck.graph.immutable

import puck.graph.AGError
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 09/06/14.
 */

abstract class NodeSet extends Iterable[NodeId] {
  type NodeType = NodeId
  type GraphT = AccessGraph
  def +(n : NodeType) : NodeSet
  def -(n : NodeType) : NodeSet

  def contains(id : NodeType) = this.iterator contains id

  def mkString(graph : AccessGraph) : String

  def scopeThatContains_*(graph : GraphT, elem: NodeType) =
    this.find { graph.contains_*(_, elem) }

  def hasScopeThatContains_*(graph : GraphT, elem: NodeType) =
    this.exists { graph.contains_*(_, elem) }

  def findCommonRoot(graph : GraphT) : NodeType = {
    val it = this.iterator
    if(!it.hasNext)
      throw new AGError("empty node set")

    def aux(root : NodeType) : NodeType = {
      if(!it.hasNext)
        root
      else{
        val n = it.next()
        if(graph.contains_*(root, n))
          aux(root)
        else if(graph.contains_*(n, root))
          aux(n)
        else
          throw new AGError("no common root in ScopeSet")
      }
    }
    aux(it.next())
  }

  def literalCopy() : LiteralNodeSet
}

object NodeSet {
  def emptySet() = LiteralNodeSet()
}

case class NamedNodeSet
( id : String,
  setDef : NodeSetDef) extends NodeSet{

  override def toString() = id

  val declare : String = "declareSet"

  def mkString(graph : GraphT) = declare + "(" + id + ", " + setDef.mkString(graph) +")."

  def iterator : Iterator[NodeType] = setDef.iterator
  def +(n : NodeType) = new NamedNodeSet(id, setDef + n)
  def -(n : NodeType) = new NamedNodeSet(id, setDef - n)
  def literalCopy() = LiteralNodeSet(setDef)
}

class NamedNodeSetUnion(id0 : String,
                        override val setDef : NodeSetUnion)
  extends NamedNodeSet(id0, setDef){
  override val declare = "declareUnionSet"
}


abstract class NodeSetDef extends NodeSet{
  def +(n : NodeType) : NodeSetDef
  def -(n : NodeType) : NodeSetDef
}

class NodeSetUnion(val sets : Seq[NodeSet],
                   val set : LiteralNodeSet) extends NodeSetDef {

  def mkString(graph : GraphT) = sets.mkString("[", ", ", ", ") +
    set.map( n => "'%s'".format(graph.getNode(n).fullName)).mkString("", ", ", "]")

  def iterator : Iterator[NodeType] = {
    val s = Seq[NodeType]() ++ set
    sets.foldLeft(s){case (s0, set0) =>  s0 ++ set0 }
    s.iterator
  }
  def +(n : NodeType) = new NodeSetUnion(sets, set + n)

  def -(n : NodeType) = throw new AGError("Do not know how to remove a node from a nodeSet union")

  def literalCopy() = LiteralNodeSet(this.iterator)
}

class NodeSetDiff(private val plus : NodeSet,
                   private val minus : NodeSet) extends NodeSetDef{

  def mkString(graph : GraphT) = plus.mkString(graph) + "\\" + minus.mkString(graph)


  def iterator : Iterator[NodeType] =
    (Set[NodeType]() ++ plus -- minus).iterator

  def +(n: NodeType) = new NodeSetDiff(plus + n, minus)
  def -(n: NodeType) = new NodeSetDiff(plus, minus + n)
  def literalCopy() = LiteralNodeSet(this.iterator)
}

class LiteralNodeSet private (private val content : Set[NodeId])
  extends NodeSetDef{

  def iterator : Iterator[NodeType] = content.iterator
  def +(n : NodeType) = new LiteralNodeSet(content + n)
  def ++ (ns : NodeSet) : LiteralNodeSet = new LiteralNodeSet(content ++ ns)
  def -(n : NodeType) = new LiteralNodeSet(content - n)

  def mkString(graph : GraphT) =
    content.map( n => "'%s'".format(graph.getNode(n).fullName)).mkString("[", ",\n", "]")
  def literalCopy() = this
}

object LiteralNodeSet{
  def apply() = new LiteralNodeSet(Set())
  def apply(n : NodeId) = new LiteralNodeSet(Set(n))
  def apply(ns : TraversableOnce[NodeId]) = {
    new LiteralNodeSet(Set() ++ ns)
  }
}