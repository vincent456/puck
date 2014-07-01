package puck.graph.backTrack

import puck.graph.constraints.{AbstractionPolicy, Constraint}
import puck.graph.{AGError, AccessGraph, AGEdge, AGNode}

import scala.collection.mutable

/**
 * Created by lorilan on 11/06/14.
 */
/*
  by default no op
 */
trait CareTaker {

  def startRegister() : CareTaker

  def stopRegister() : CareTaker

  def sequence[T]( op : => T ) : T

  def undo() : Transformation

  val recording : Recording

  def addNode(n: AGNode)

  def removeNode(n: AGNode)

  def addEdge(e: AGEdge)

  def removeEdge(e: AGEdge)

  def addEdgeDependency(dominant: AGEdge, dominated: AGEdge)

  def removeEdgeDependency(dominant: AGEdge, dominated: AGEdge)

  def registerAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy)
  def unregisterAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy)

  def addFriend(ct : Constraint, friend : AGNode)
}

class Recording( private [backTrack] val graph : AccessGraph,
                 private [backTrack] val registering : Int,
                 private [backTrack] val sequences : List[CompositeTransformation]){
  def redo(){sequences.foreach(_.redo())}
}

class CareTakerNoop(val graph : AccessGraph) extends CareTaker{

  def startRegister()={
    graph.transformations = new CareTakerRegister(graph)
    graph.transformations
  }

  def stopRegister()={
    graph.transformations = this
    this
  }
  val emptyRecording = new Recording(graph, 0, List())

  def recording = emptyRecording

  def recording_=(r : Recording){}

  def sequence[T]( op : => T ) = op

  def undo() = new CompositeTransformation()

  def addNode(n: AGNode){}

  def removeNode(n: AGNode) {}

  def addEdge(e: AGEdge){}

  def removeEdge(e: AGEdge){}

  def addEdgeDependency(dominant: AGEdge, dominated: AGEdge) {}
  def removeEdgeDependency(dominant: AGEdge, dominated: AGEdge) {}

  def registerAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy){}
  def unregisterAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy){}

  def addFriend(ct : Constraint, friend : AGNode){}
}

class CareTakerRegister (val graph : AccessGraph) extends CareTaker {

  private [this] var registering = 1

  def startRegister()={
    registering += 1
    graph.transformations = this
    this
  }

  def stopRegister()={
    registering -= 1
    if(registering == 0 ){
      graph.transformations = new CareTakerNoop(graph)
    }
    graph.transformations
  }

  private val sequencesStack = new mutable.Stack[CompositeTransformation]()
  sequencesStack.push(new CompositeTransformation())

  private def currentSequence : CompositeTransformation = sequencesStack.head

  def +=(t : Transformation) {
    currentSequence.push(t)
  }

  def recording = new Recording(graph, registering, sequencesStack.reverseIterator.toList)

  def recording_=(r : Recording){
    if(graph != r.graph)
      throw new AGError("Illegal recording !")

    this.registering = r.registering

    sequencesStack.clear()
    r.sequences.foreach(sequencesStack.push)
  }

  def sequence[T]( op : => T ) = {
    var seq = new CompositeTransformation()
    this += seq
    sequencesStack.push(seq)

    val res = op

    sequencesStack.pop()

    res
  }

  def undo() {
    graph.transformations = new CareTakerNoop(graph)
    /*println("sequence to undo : ")
    currentSequence.sequence.foreach(println)
    println("end of sequence to undo.")*/
    val undoSeq = sequencesStack.pop()
    sequencesStack.push(new CompositeTransformation)

    undoSeq.undo()
    undoSeq
  }

  def addNode(n: AGNode) {
    this += new AddNode(n)
  }

  def removeNode(n: AGNode) {
    this += new RemoveNode(n)
  }

  def addEdge(e: AGEdge) {
    this += new AddEdge(e)
  }

  def removeEdge(e: AGEdge) {
    this += new RemoveEdge(e)
  }

  def addEdgeDependency(dominant: AGEdge, dominated: AGEdge) {
    this += new AddEdgeDependency(dominant, dominated)
  }

  def removeEdgeDependency(dominant: AGEdge, dominated: AGEdge) {
    this += new RemoveEdgeDependency(dominant, dominated)
  }
  
  def registerAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy) {
    this += new RegisterAbstraction(impl, abs, policy)
  }

  def unregisterAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy) {
    this += new UnregisterAbstraction(impl, abs, policy)
  }
  def addFriend(ct : Constraint, friend : AGNode){
    this += new AddFriend(ct, friend)
  }
}