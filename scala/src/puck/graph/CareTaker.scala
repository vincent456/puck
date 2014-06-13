package puck.graph

import puck.graph.constraints.AbstractionPolicy

import scala.collection.mutable

/**
 * Created by lorilan on 11/06/14.
 */
/*
  by default no op
 */
trait CareTaker {

  def startRegister()

  def stopRegister()

  def sequence[T]( op : => T ) : T

  def undo()

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
}

class CareTakerNoop(val graph : AccessGraph) extends CareTaker{

  def startRegister(){graph.transformations = new CareTakerRegister(graph)}

  def stopRegister(){graph.transformations = this}

  def sequence[T]( op : => T ) = op

  def undo(){}

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
}

class CareTakerRegister(val graph : AccessGraph) extends CareTaker {

  def startRegister(){graph.transformations = this}

  def stopRegister(){graph.transformations = new CareTakerNoop(graph)}

  private val sequencesStack = new mutable.Stack[CompositeTransformation]()
  sequencesStack.push(new CompositeTransformation())

  private def currentSequence : CompositeTransformation = sequencesStack.head

  def +=(t : Transformation) {
    currentSequence.push(t)
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
    stopRegister()
    currentSequence.undo()
    startRegister()
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
}