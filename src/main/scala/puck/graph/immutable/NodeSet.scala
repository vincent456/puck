package puck.graph.immutable

import puck.graph.AGError
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 09/06/14.
 */

abstract class NodeSet[Kind <: NodeKind[Kind]] extends Iterable[NodeId[Kind]] {
  type NodeType = NodeId[Kind]
  type GraphT = AccessGraph[Kind, _]
  def +(n : NodeType) : NodeSet[Kind]
  def -(n : NodeType) : NodeSet[Kind]

  def contains(id : NodeType) = this.iterator contains id

  def mkString(graph : AccessGraph[Kind, _]) : String

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

  def literalCopy() : LiteralNodeSet[Kind]
}

object NodeSet {
  def emptySet[Kind <: NodeKind[Kind]]() = LiteralNodeSet[Kind]()
}

case class NamedNodeSet[Kind <: NodeKind[Kind]]
( id : String,
  setDef : NodeSetDef[Kind]) extends NodeSet[Kind]{

  override def toString() = id

  val declare : String = "declareSet"

  def mkString(graph : GraphT) = declare + "(" + id + ", " + setDef.mkString(graph) +")."

  def iterator : Iterator[NodeType] = setDef.iterator
  def +(n : NodeType) = new NamedNodeSet(id, setDef + n)
  def -(n : NodeType) = new NamedNodeSet(id, setDef - n)
  def literalCopy() = LiteralNodeSet(setDef)
}

class NamedNodeSetUnion[Kind <: NodeKind[Kind]](id0 : String,
                        override val setDef : NodeSetUnion[Kind])
  extends NamedNodeSet[Kind](id0, setDef){
  override val declare = "declareUnionSet"
}


abstract class NodeSetDef[Kind <: NodeKind[Kind]] extends NodeSet[Kind]{
  def +(n : NodeType) : NodeSetDef[Kind]
  def -(n : NodeType) : NodeSetDef[Kind]
}

class NodeSetUnion[Kind <: NodeKind[Kind]](val sets : Seq[NodeSet[Kind]],
                   val set : LiteralNodeSet[Kind]) extends NodeSetDef[Kind]{

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

class NodeSetDiff[Kind <: NodeKind[Kind]](private val plus : NodeSet[Kind],
                   private val minus : NodeSet[Kind]) extends NodeSetDef[Kind]{

  def mkString(graph : GraphT) = plus.mkString(graph) + "\\" + minus.mkString(graph)


  def iterator : Iterator[NodeType] =
    (Set[NodeType]() ++ plus -- minus).iterator

  def +(n: NodeType) = new NodeSetDiff(plus + n, minus)
  def -(n: NodeType) = new NodeSetDiff(plus, minus + n)
  def literalCopy() = LiteralNodeSet(this.iterator)
}

class LiteralNodeSet[Kind <: NodeKind[Kind]] private (private val content : Set[NodeId[Kind]])
  extends NodeSetDef[Kind]{

  def iterator : Iterator[NodeType] = content.iterator
  def +(n : NodeType) = new LiteralNodeSet(content + n)
  def ++ (ns : NodeSet[Kind]) : LiteralNodeSet[Kind] = new LiteralNodeSet(content ++ ns)
  def -(n : NodeType) = new LiteralNodeSet(content - n)

  def mkString(graph : GraphT) =
    content.map( n => "'%s'".format(graph.getNode(n).fullName)).mkString("[", ",\n", "]")
  def literalCopy() = this
}

object LiteralNodeSet{
  def apply[Kind <: NodeKind[Kind]]() = new LiteralNodeSet[Kind](Set())
  def apply[Kind <: NodeKind[Kind]](n : NodeId[Kind]) = new LiteralNodeSet[Kind](Set(n))
  def apply[Kind <: NodeKind[Kind]](ns : TraversableOnce[NodeId[Kind]]) = {
    new LiteralNodeSet[Kind](Set() ++ ns)
  }
}