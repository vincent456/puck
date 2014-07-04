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
class Recording( private [backTrack] val graph : AccessGraph,
                 private [backTrack] val registering : Int,
                 private [backTrack] val composition : List[Transformation]){
  def redo(){composition.foreach(_.redo())}
  def undo(){composition.reverseIterator.foreach(_.undo())}


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
    (composition.length == other.composition.length) &&
    partialGraph().softEqual(other.partialGraph())
    /*val mapping = Map[AGNode, AGNode]()

    def normalizeNodeTransfos(l : List[Transformation]) = {
      val (m2, l2) = l.foldLeft( (Map[AGNode, Int](), List[Transformation]()) ){
        case ((m,l1), AddNode(n))=>
          val i = m.getOrElse(n, 0)
          (m + (n -> i + 1), l1)
        case ((m,l1), RemoveNode(n)) =>
          val i = m.getOrElse(n, 0)
          (m + (n -> i - 1), l1)
        case ((m,l1),t) => (m, t :: l1)
      }

      (m2, l2.reverse)
    }

    (composition.length == other.composition.length) && {

      def predicate = {case AddNode(_) | RemoveNode(_) => true
      case _ => false}

      val (nodeMap, remainingTransfos) = normalizeNodeTransfos(composition)

      val (nodeTransfosOther, remainingTransfosOther) = normalizeNodeTransfos(other.composition)




    }*/
  }
}

object EmptyRecording extends Recording(null, 0, null){
  override def redo(){}
  override def undo(){}
}

object Recording{
  def apply(g : AccessGraph, r: Int, s : mutable.Stack[Transformation]) = {

    val buf = mutable.ListBuffer[Transformation]()

    s.reverseIterator.foreach{ t =>
      //println("copying " + t)
      buf += t.copy()
    }

    new Recording(g,r,buf.toList)
  }
}



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

  private val transformationsStack = new mutable.Stack[Transformation]()

  private def +=(t : Transformation) {
    transformationsStack.push(t)
  }

  def recording = {
    Recording(graph, registering, transformationsStack)
  }

  def recording_=(r : Recording){
    if(graph != r.graph)
      throw new AGError("Illegal recording !")

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

  override def recording = EmptyRecording

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