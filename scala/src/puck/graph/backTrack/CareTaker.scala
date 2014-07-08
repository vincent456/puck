package puck.graph.backTrack

import puck.graph.constraints.{AbstractionPolicy, Constraint}
import puck.graph.{AGError, AccessGraph, AGEdge, AGNode}

import scala.collection.mutable

class CareTaker (val graph : AccessGraph) {

  private [this] var registering = 1

  def register[T](op : => T) : T = {
    startRegister()
    val res = sequence[T](op)
    stopRegister()
    res
  }

  def startRegister()={
    registering += 1

    graph.transformations = this
    this
  }

  def stopRegister()={
    registering -= 1

    if(registering == 0 )
      graph.transformations = new CareTakerNoop(graph)

    graph.transformations
  }

  private val transformationsStack = new mutable.Stack[Recordable]()

  private def +=(t : Transformation) {
    transformationsStack.push(t)
  }

  def recording = {
    Recording(graph, registering, transformationsStack)
  }

  def recording_=(r : Recording){
    if(graph != r.graph)
      throw new AGError("Illegal recording !")

    r.redo()


    this.registering = r.registering
    transformationsStack.clear()
    r.composition.foreach(transformationsStack.push)
  }


  def startSequence(){
    transformationsStack.push(UndoBreakPoint())
  }


  def sequence[T]( op : => T ) = {
    startSequence()
    op
  }

  def undo(breakPoint : BreakPoint = UndoBreakPoint()) = {

    graph.transformations = new CareTakerNoop(graph)

    while(transformationsStack.nonEmpty &&
        transformationsStack.head != breakPoint)
        transformationsStack.pop().undo()

    graph.transformations = this
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

class CareTakerNoop(g : AccessGraph) extends CareTaker(g){

  override def register[T](op : => T) : T = op

  override def startRegister()={
    graph.transformations = new CareTaker(graph)
    graph.transformations
  }

  override def stopRegister()={
    graph.transformations = this
    this
  }

  override def recording = Recording.empty(g)

  override def recording_=(r : Recording){}

  override def sequence[T]( op : => T ) = op

  override def startSequence(){}

  override def undo(breakPoint : BreakPoint) {}

  override def addNode(n: AGNode){}

  override def removeNode(n: AGNode) {}

  override def addEdge(e: AGEdge){}

  override def removeEdge(e: AGEdge){}

  override def addEdgeDependency(dominant: AGEdge, dominated: AGEdge) {}
  override def removeEdgeDependency(dominant: AGEdge, dominated: AGEdge) {}

  override def registerAbstraction(impl: AGNode, abs: AGNode,
                          policy: AbstractionPolicy){}
  override def unregisterAbstraction(impl: AGNode, abs: AGNode,
                            policy: AbstractionPolicy){}

  override def addFriend(ct : Constraint, friend : AGNode){}
}