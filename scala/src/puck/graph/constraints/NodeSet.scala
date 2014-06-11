package puck.graph.constraints

import puck.graph.{AGError, AGNode}
import scala.collection.mutable

/**
 * Created by lorilan on 09/06/14.
 */

abstract class NodeSet extends mutable.Iterable[AGNode] {
  def +=(n : AGNode)
  def -=(n : AGNode)

  def scopeThatContains_*(elem: AGNode) = this.find { _.contains_*(elem) }
  def hasScopeThatContains_*(elem: AGNode) = this.exists { _.contains_*(elem) }

  def findCommonRoot : AGNode = {
    val it = this.iterator
    if(!it.hasNext)
      throw new AGError("empty node set")

    def aux(root : AGNode) : AGNode = {
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

  def literalCopy() : LiteralNodeSet
}

object NodeSet {
  def emptySet() = LiteralNodeSet()
}

class NamedNodeSet(val id : String,
                   val setDef : NodeSetDef) extends NodeSet{

  override def toString() = id

  val declare : String = "declareSet"

  def defString = declare + "(" + id + ", " + setDef +")."


  def iterator : Iterator[AGNode] = setDef.iterator
  def +=(n : AGNode) = setDef += n
  def -=(n : AGNode) = setDef -= n
  def literalCopy() = LiteralNodeSet(setDef)
}

class NamedNodeSetUnion(id0 : String,
                        override val setDef : NodeSetUnion)
  extends NamedNodeSet(id0, setDef){
  override val declare = "declareUnionSet"
}


abstract class NodeSetDef extends NodeSet

class NodeSetUnion(val sets : mutable.Buffer[NodeSet],
                   val set : LiteralNodeSet) extends NodeSetDef{

  override def toString() = sets.mkString("[", ", ", ", ") +
    set.mkString("", ", ", "]")

  def iterator : Iterator[AGNode] = {
    val s = mutable.Set[AGNode]()
    s ++= set
    sets.foreach( s0 => s ++= s0 )
    s.iterator
  }
  def +=(n : AGNode) = set += n

  def -=(n : AGNode) = throw new AGError("Do not know how to remove a node from a nodeSet union")

  def literalCopy() = LiteralNodeSet(this.iterator)
}

class NodeSetDiff(private val plus : NodeSet,
                   private val minus : NodeSet) extends NodeSetDef{

  override def toString() = plus.mkString("[", ",\n", "]\\") + minus.mkString("[", ",\n", "]")

  def iterator : Iterator[AGNode] = {
    val s = mutable.Set[AGNode]()
    s ++= plus
    s --= minus
    s.iterator
  }
  def +=(n: AGNode) = plus += n
  def -=(n: AGNode) = minus += n
  def literalCopy() = LiteralNodeSet(this.iterator)
}

class LiteralNodeSet private (private val content : mutable.Set[AGNode])
  extends NodeSetDef{

  def iterator : Iterator[AGNode] = content.iterator
  def +=(n : AGNode) = content += n
  def -=(n : AGNode) = content -= n

  override def toString() = content.mkString("[", ",\n", "]")
  def literalCopy() = LiteralNodeSet(this.iterator)
}

object LiteralNodeSet{
  def apply() = new LiteralNodeSet(mutable.Set())
  def apply(n : AGNode) = new LiteralNodeSet(mutable.Set(n))
  def apply(ns : TraversableOnce[AGNode]) = {
    val s = mutable.Set[AGNode]()
    s ++= ns
    new LiteralNodeSet(s)
  }
}