package puck.graph.backTrack

import puck.graph.{AGEdge, AGError, AccessGraph, AGNode}
import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */
object Recording{
  def apply(g : AccessGraph, r: Int, s : mutable.Stack[Recordable]) = {

    val buf = mutable.ListBuffer[Recordable]()

    s.reverseIterator.foreach{ t =>
      //println("copying " + t)
      buf += t.copy()
    }

    new Recording(g,r,buf.toList)
  }

  def empty(g : AccessGraph) = new Recording(g, 0, List())
}

/**
 * Created by lorilan on 11/06/14.
 */
/*
  by default no op
 */
class Recording( val graph : AccessGraph,
                 private [backTrack] val registering : Int,
                 private [backTrack] val composition : List[Recordable]){

  def apply(){graph(this)}

  def redo(){composition.foreach(_.redo())}
  def undo(){composition.reverseIterator.foreach(_.undo())}

  def isEmpty = composition.isEmpty
  def nonEmpty = composition.nonEmpty

  def partialGraph() : AccessGraph = {
    val g = graph.newGraph()
    val map = mutable.Map[AGNode, AGNode]()
    map += (graph.root -> g.root)

    def get(n : AGNode) =
      map.getOrElse(n, g.addNode(n.name, n.kind))

    composition.foreach{
      case AddNode(n) =>
        val n2 = get(n)
        map += (n -> n2)
        g.addNode(n2)
      case RemoveNode(n) =>
        throw new AGError("partial graph remove node should not happen")
      case AddEdge(e) =>
        AGEdge(e.kind, get(e.source), get(e.target)).create()
      case RemoveEdge(e) =>
        AGEdge(e.kind, get(e.source), get(e.target)).delete()
      case _ => ()
    }

    g
  }

  def produceSameGraph(other : Recording) : Boolean = {
    composition.length == other.composition.length && {
       new RecordingComparator(this, other).search() match {
         case None => false
         case Some(_) => true
       }
    }
  }
}


