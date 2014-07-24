package puck.graph.constraints

import puck.graph.{NodeKind, AGError, AGNode}
import scala.collection.mutable

/**
 * Created by lorilan on 09/06/14.
 */

abstract class NodeSet[Kind <: NodeKind[Kind]] extends mutable.Iterable[AGNode[Kind]] {
  type NodeType = AGNode[Kind]
  def +=(n : NodeType)
  def -=(n : NodeType)

  def scopeThatContains_*(elem: NodeType) = this.find { _.contains_*(elem) }
  def hasScopeThatContains_*(elem: NodeType) = this.exists { _.contains_*(elem) }

  def findCommonRoot : NodeType = {
    val it = this.iterator
    if(!it.hasNext)
      throw new AGError("empty node set")

    def aux(root : NodeType) : NodeType = {
      if(!it.hasNext)
        root
      else{
        val n = it.next()
        if(root.contains_*(n))
          aux(root)
        else if(n.contains_*(root))
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

class NamedNodeSet[Kind <: NodeKind[Kind]](val id : String,
                   val setDef : NodeSetDef[Kind]) extends NodeSet[Kind]{

  override def toString() = id

  val declare : String = "declareSet"

  def defString = declare + "(" + id + ", " + setDef +")."


  def iterator : Iterator[NodeType] = setDef.iterator
  def +=(n : NodeType) = setDef += n
  def -=(n : NodeType) = setDef -= n
  def literalCopy() = LiteralNodeSet(setDef)
}

class NamedNodeSetUnion[Kind <: NodeKind[Kind]](id0 : String,
                        override val setDef : NodeSetUnion[Kind])
  extends NamedNodeSet[Kind](id0, setDef){
  override val declare = "declareUnionSet"
}


abstract class NodeSetDef[Kind <: NodeKind[Kind]] extends NodeSet[Kind]

class NodeSetUnion[Kind <: NodeKind[Kind]](val sets : mutable.Buffer[NodeSet[Kind]],
                   val set : LiteralNodeSet[Kind]) extends NodeSetDef[Kind]{

  override def toString() = sets.mkString("[", ", ", ", ") +
    set.map( n => "'%s'".format(n.fullName)).mkString("", ", ", "]")

  def iterator : Iterator[NodeType] = {
    val s = mutable.Set[NodeType]()
    s ++= set
    sets.foreach( s0 => s ++= s0 )
    s.iterator
  }
  def +=(n : NodeType) = set += n

  def -=(n : NodeType) = throw new AGError("Do not know how to remove a node from a nodeSet union")

  def literalCopy() = LiteralNodeSet(this.iterator)
}

class NodeSetDiff[Kind <: NodeKind[Kind]](private val plus : NodeSet[Kind],
                   private val minus : NodeSet[Kind]) extends NodeSetDef[Kind]{

  override def toString() = plus.mkString("[", ",\n", "]\\") + minus.mkString("[", ",\n", "]")


  def iterator : Iterator[NodeType] = {
    val s = mutable.Set[NodeType]()
    s ++= plus
    s --= minus
    s.iterator
  }
  def +=(n: NodeType) = plus += n
  def -=(n: NodeType) = minus += n
  def literalCopy() = LiteralNodeSet(this.iterator)
}

class LiteralNodeSet[Kind <: NodeKind[Kind]] private (private val content : mutable.Set[AGNode[Kind]])
  extends NodeSetDef[Kind]{

  def iterator : Iterator[NodeType] = content.iterator
  def +=(n : NodeType) = content += n
  def -=(n : NodeType) = content -= n

  override def toString() = content.map( n => "'%s'".format(n.fullName)).mkString("[", ",\n", "]")
  def literalCopy() = LiteralNodeSet(this.iterator)
}

object LiteralNodeSet{
  def apply[Kind <: NodeKind[Kind]]() = new LiteralNodeSet[Kind](mutable.Set())
  def apply[Kind <: NodeKind[Kind]](n : AGNode[Kind]) = new LiteralNodeSet[Kind](mutable.Set(n))
  def apply[Kind <: NodeKind[Kind]](ns : TraversableOnce[AGNode[Kind]]) = {
    val s = mutable.Set[AGNode[Kind]]()
    s ++= ns
    new LiteralNodeSet[Kind](s)
  }
}