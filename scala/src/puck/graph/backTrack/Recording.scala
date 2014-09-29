package puck.graph.backTrack

import puck.graph._
import scala.collection.mutable

/**
 * Created by lorilan on 07/07/14.
 */
object Recording{
  def apply[Kind <: NodeKind[Kind]](g : AccessGraph[Kind], r: Int, s : mutable.Stack[Recordable[Kind]]) = {

    val buf = mutable.ListBuffer[Recordable[Kind]]()

    s.reverseIterator.foreach{ t =>
      //println("copying " + t)
      buf += t.copy()
    }

    new Recording(g,r,buf.toList)
  }

  def empty[Kind <: NodeKind[Kind]](g : AccessGraph[Kind]) = new Recording[Kind](g, 0, List())
}

/**
 * Created by lorilan on 11/06/14.
 */
/*
  by default no op
 */
class Recording[Kind <: NodeKind[Kind]]( val graph : AccessGraph[Kind],
                 private [backTrack] val registering : Int,
                 private [backTrack] val composition : List[Recordable[Kind]])
  extends Iterable[Recordable[Kind]]{

  override def toString =
  composition.mkString("Recording(",",\n", ")\n")

  def iterator = composition.iterator

  def apply(){graph(this)}

  def redo(){composition.foreach(_.redo())}
  def undo(){
    val ct =  graph.transformations
    graph.transformations = new CareTakerNoop(graph)
    composition.reverseIterator.foreach(_.undo())
    graph.transformations = ct
  }

  def partialGraph() : AccessGraph[Kind] = {
    val g = graph.newGraph()
    val map = mutable.Map[AGNode[Kind], AGNode[Kind]]()
    map += (graph.root -> g.root)

    def get(n : AGNode[Kind]) =
      map.getOrElse(n, g.addNode(n.name, n.kind))

    composition.foreach{
      case Transformation(Add(), TTNode(n)) =>
        val n2 = get(n)
        map += (n -> n2)
        g.addNode(n2)
      case Transformation(Remove(), TTNode(n)) =>
        throw new AGError("partial graph remove node should not happen")
      case Transformation(Add(), TTEdge(e)) =>
        AGEdge(e.kind, get(e.source), get(e.target)).create()
      case Transformation(Remove(), TTEdge(e)) =>
        AGEdge(e.kind, get(e.source), get(e.target)).delete()
      case _ => ()
    }

    g
  }

  def produceSameGraph(other : Recording[Kind]) : Boolean = {
    composition.length == other.composition.length && {
       new RecordingComparator(this, other).search() match {
         case None => false
         case Some(_) => true
       }
    }
  }
}


