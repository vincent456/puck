package puck.control
import puck.graph.Recording.RecordingOps
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.MileStone
import puck.graph.{DependencyGraph, Recording}
import puck.view.SearchSpacePane

import scala.swing.{Component, Publisher}
import scala.collection.mutable
/**
  * Created by Loïc Girault on 12/8/16.
  */
class SearchSpace
(control : PuckControl
) extends HistoryHandler{

  val bus : Publisher = control.Bus
  def graph: DependencyGraph = top.graph
  private [this] var top : Node = _
  private [this] var sroot : Option[Node] = _

  def setInitialGraph(g: DependencyGraph): Unit = {
    val r = new Node(g)
    sroot = Some(r)
    top = r
  }

  private def push(graph: DependencyGraph) : Node = {
    val n = new Node(graph, Some(top))
    top.children.append(n)
    val previousTop = top
    top = n
    previousTop
  }

  private def pop() : Node = top.parent match {
    case None => puck.error("already at root !")
    case Some(parent) =>
      val idx = parent.children.indexOf(top)
      val oldTop = parent.children.remove(idx)
      top = parent
      oldTop
  }

  def pushConstraints(cm: ConstraintsMaps ) = {
    push(graph.mileStone.constraintChange(cm))
    bus publish ConstraintsUpdateRequest(cm)
  }

  def pushGraph(graph: DependencyGraph): Unit = {
    val previousTop = push(graph)
    firePushEvent(previousTop.graph)
  }

  def rewriteHistory(rec: Recording): Unit = {
    pop()
    pushGraph(rec redo graph)
  }

  def load(rec: Recording): Unit = {
    val g = rec.reverse.foldLeft(graph) {
      case (g, MileStone) =>
        push(g)
        MileStone.redo(g)
      case (g, t) => t.redo(g)
    }
    if(!(g eq graph))
      push(g)

    bus publish GraphUpdate(graph)
  }

  def view(): Component = new SearchSpacePane(control, this)

  def toDot(fit : DependencyGraph => Int) : String = {
    val g = sroot map (_.toDot(top.graph.id, fit)) getOrElse """0 [label = "no graph"]"""
    //println(g)
    s"digraph G{ rankdir=LR; ranksep=equally; $g }"
  }

  def setTop(n : Node) : Unit = {
    top = n
    fireUpdate()
  }

  def findNode(id : Int, node : Node) : Option[Node] =
    if(node.graph.id == id) Some(node)
    else node.children.flatMap(findNode(id, _)).headOption


  def setAsCurrentGraph(id : Int) : Unit =
    for {
      root <- sroot
      n <- findNode(id,root)
    } setTop(n)
}

class Node
(val graph : DependencyGraph,
 val parent : Option[Node] = None,
 val children : mutable.ListBuffer[Node] = mutable.ListBuffer()) {

  def toDot(current : Int, fit : DependencyGraph => Int) : String = {
   println(graph.id + ", children = " + children)
    val lbl0 = s"id= ${graph.id}, m = ${fit(graph)}"
    val lbl =
      if(graph.id == current) s"<<B>$lbl0</B>>"
      else s""""$lbl0""""
    s"""${graph.id} [label = $lbl URL="${graph.id}"];""" +
      (children map {
        n =>
          val cmt = "" // n.graph.recording.commentsSinceLastMileStone.head
          s"""${n.toDot(current, fit)} ${graph.id} -> ${n.graph.id} [label = "$cmt"];"""
      } mkString "\n")
  }

}
