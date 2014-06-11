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

  def sequence[T]( op : => T ) : T

  def undo()

  def addNode(n: AGNode)

  def removeNode(n: AGNode)

  def addEdge(e: AGEdge)

  def removeEdge(e: AGEdge)

  def addEdgeDependancy(dominant: AGEdge, dominated: AGEdge)

  def registerAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy)
}

class CareTakerNoop extends CareTaker{
  def sequence[T]( op : => T ) = op

  def undo(){}

  def addNode(n: AGNode){}

  def removeNode(n: AGNode) {}

  def addEdge(e: AGEdge){}

  def removeEdge(e: AGEdge){}

  def addEdgeDependancy(dominant: AGEdge, dominated: AGEdge) {}

  def registerAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy){}
}

class CareTakerRegister extends CareTaker {

  private val registeredModifs = new mutable.Stack[Transformation]()

  private var currentSequence : CompositeTransformation = _
  private var sequenceOnGoing = false

  def +=(t : Transformation) {
    currentSequence.push(t)
  }

  def sequence[T]( op : => T ) = {
    if(sequenceOnGoing)     // stack of on going sequence ???
      throw new AGError("Cannot start a modification sequence while one is on going")

    currentSequence = new CompositeTransformation()
    sequenceOnGoing = true
    val res = op
    registeredModifs.push(currentSequence)
    sequenceOnGoing = false
    res
  }

  def undo() {
    registeredModifs.pop().undo()
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

  def addEdgeDependancy(dominant: AGEdge, dominated: AGEdge) {
    this += new AddEdgeDependancy(dominant, dominated)
  }

  def registerAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy) {
    this += new RegisterAbstraction(impl, abs, policy)
  }

}