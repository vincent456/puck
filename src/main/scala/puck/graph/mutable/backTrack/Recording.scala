package puck.graph.mutable.backTrack

import puck.graph.mutable.backTrack.comparison.RecordingComparator
import puck.graph.mutable.{AccessGraph, NodeKind}
import puck.util._
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

    new Recording(g, r, buf.toList)
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
                 /*private [backTrack]*/ val composition : List[Recordable[Kind]])
  extends Iterable[Recordable[Kind]]{

  override def toString = composition.mkString("Recording(",",\n", ")\n")

  def iterator = composition.iterator

  def apply(){graph(this)}

  def redo(){
    val careTaker = graph.transformations
    graph.transformations = new CareTakerNoop(graph)

    composition foreach { _.redo() }
    graph.transformations = careTaker
  }

  def undo(){
    val careTaker = graph.transformations
    graph.transformations = new CareTakerNoop(graph)
    composition.reverseIterator foreach { _.undo() }
    graph.transformations = careTaker
  }

  def produceSameGraph(other : Recording[Kind],
                       logger : PuckLogger = PuckNoopLogger) : Boolean = {


    val engine = new RecordingComparator(graph.initialRecord, this, other)
    engine.search()
    if(engine.finalStates.isEmpty) {
      logger.writeln("no mapping")((PuckLog.NoSpecialContext, PuckLog.Debug))
      false
    }
    else {
      logger.writeln(engine.finalStates.head.result.toString())(PuckLog.NoSpecialContext, PuckLog.Debug)
      true
    }
  }

}


