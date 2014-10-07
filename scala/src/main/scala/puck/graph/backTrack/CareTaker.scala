package puck.graph.backTrack

import puck.graph.constraints.{AbstractionPolicy, Constraint}
import puck.graph._

import scala.collection.mutable

class CareTaker[Kind <: NodeKind[Kind]] (val graph : AccessGraph[Kind]) {

  private [this] var registering = 1

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

  private val transformationsStack = new mutable.Stack[Recordable[Kind]]()

  private def +=(t : Transformation[Kind]) {
    transformationsStack.push(t)
  }

  def recording = {
    Recording(graph, registering, transformationsStack)
  }

  def recording_=(r : Recording[Kind]){
    if(graph != r.graph)
      throw new AGError("Illegal recording !")

    r.redo()

    this.registering = r.registering
    transformationsStack.clear()
    r.composition foreach transformationsStack.push
  }


  def startSequence(){
    transformationsStack.push(UndoBreakPoint[Kind]())
  }


  def sequence[T]( op : => T ) = {
    startSequence()
    op
  }

  def undo(breakPoint : BreakPoint[Kind] = UndoBreakPoint()) = {

    graph.transformations = new CareTakerNoop(graph)

    while(transformationsStack.nonEmpty &&
      transformationsStack.head != breakPoint)
      transformationsStack.pop().undo()

    graph.transformations = this
  }

  type NodeType = AGNode[Kind]
  type EdgeType = AGEdge[Kind]

  def addNode(n: NodeType) {
    this += Transformation(Add(), TTNode(n))
  }

  def removeNode(n: NodeType) {
    this += Transformation(Remove(), TTNode(n))
  }

  def addEdge(e: EdgeType) {
    this += Transformation(Add(), TTEdge(e))
  }

  def removeEdge(e: EdgeType) {
    this += Transformation(Remove(), TTEdge(e))
  }

  def addEdgeDependency(dominant: EdgeType, dominated: EdgeType) {
    this += Transformation(Add(), TTDependency(dominant, dominated))
  }

  def removeEdgeDependency(dominant: EdgeType, dominated: EdgeType) {
    this += Transformation(Remove(), TTDependency(dominant, dominated))
  }

  def createAbstraction(impl: NodeType, abs: NodeType,
                        policy: AbstractionPolicy) {
    this += Transformation(Add(), TTAbstraction(impl, abs, policy))
    this += Transformation(Add(), TTNode(abs))
  }

  def registerAbstraction(impl: NodeType, abs: NodeType,
                          policy: AbstractionPolicy) {
    this += Transformation(Add(), TTAbstraction(impl, abs, policy))
  }

  def unregisterAbstraction(impl: NodeType, abs: NodeType,
                            policy: AbstractionPolicy) {
    this += Transformation(Remove(), TTAbstraction(impl, abs, policy))
  }
  def addFriend(ct : Constraint[Kind], friend : NodeType){
    this += Transformation(Add(), TTConstraint(ct, friend))
  }

  def changeEdgeTarget(e : AGEdge[Kind], newTarget : AGNode[Kind]){
    this += Transformation(Add(), TTRedirection(e, Target(newTarget)))
  }

  def changeEdgeSource(e : AGEdge[Kind], newSource : AGNode[Kind]){
    this += Transformation(Add(), TTRedirection(e, Source(newSource)))
  }

  def changeType[T <: Type[Kind, T]](kind : HasType[Kind, T], oldUsee : AGNode[Kind], newUsee : AGNode[Kind]){
    this += Transformation(Add(), TTTypeRedirection(kind, oldUsee, newUsee))
  }
}

class CareTakerNoop[Kind <: NodeKind[Kind]](g : AccessGraph[Kind]) extends CareTaker[Kind](g){

  override def startRegister()={
    graph.transformations = new CareTaker(graph)
    graph.transformations
  }

  override def stopRegister()={
    graph.transformations = this
    this
  }

  override def recording = Recording.empty(g)

  override def recording_=(r : Recording[Kind]){}

  override def sequence[T]( op : => T ) = op

  override def startSequence(){}

  override def undo(breakPoint : BreakPoint[Kind]){}

  override def addNode(n: NodeType){}

  override def removeNode(n: NodeType) {}

  override def addEdge(e: EdgeType){}

  override def removeEdge(e: EdgeType){}

  override def addEdgeDependency(dominant: EdgeType, dominated: EdgeType){}

  override def removeEdgeDependency(dominant: EdgeType, dominated: EdgeType) {}

  override def registerAbstraction(impl: NodeType, abs: NodeType,
                                   policy: AbstractionPolicy){}
  override def unregisterAbstraction(impl: NodeType, abs: NodeType,
                                     policy: AbstractionPolicy){}

  override def addFriend(ct : Constraint[Kind], friend : NodeType){}

  override def changeEdgeTarget(e : AGEdge[Kind], newTarget : AGNode[Kind]){}
  override def changeEdgeSource(e : AGEdge[Kind], newSource : AGNode[Kind]){}

  override def changeType[T <: Type[Kind, T]](kind : HasType[Kind, T], oldUsee : AGNode[Kind], newUsee : AGNode[Kind]){}
}