package puck.graph

import puck.graph.backTrack.{Transformation, Recording, CareTakerNoop, CareTaker}
import puck.util.{NoopLogger, Logger}

import scala.language.implicitConversions
import scala.collection.mutable
import puck.graph.constraints.{Constraint, NamedNodeSet}

/**
 * Created by lorilan on 05/05/14.
 */

object AccessGraph {
  val rootId = 0
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"

  implicit def agToIterator[Kind <: NodeKind[Kind]](ag : AccessGraph[Kind]) : Iterator[AGNode[Kind]] = ag.iterator

}

class AccessGraph[Kind <: NodeKind[Kind]] (nodeBuilder : AGNodeBuilder[Kind]) {
  //extends Iterable[AGNode]{ //is NOT iterable (see scala doc for requirements) but
  // has a valid iterator + implicit conversion in puck.graph package object

  type NodeType = AGNode[Kind]
  type EdgeType = AGEdge[Kind]

  var logger : Logger[Int] = new NoopLogger()

  def newGraph() : AccessGraph[Kind] = {
    new AccessGraph(nodeBuilder)
  }

  logger.writeln("Node builder : " + nodeBuilder.getClass)

  val nodeSets : mutable.Map[String, NamedNodeSet[Kind]] = mutable.Map()
  val constraints : mutable.Buffer[Constraint[Kind]] = mutable.Buffer()

  private [this] val nodes0 = mutable.Buffer[NodeType]()

  def nodes : Iterator[NodeType] = nodes0.iterator

  def iterator = root.iterator

  private var id : Int = 1
  val root : NodeType = nodeBuilder(this, AccessGraph.rootId, AccessGraph.rootName, nodeBuilder.rootKind)

  val scopeSeparator = nodeBuilder.scopeSeparator

  /*override def hashCode: Int = this.id * 42 // ???
  */

  def nodeKinds = nodeBuilder.kinds

  def violations : List[EdgeType] = {
    this.foldLeft(List[EdgeType]()){
      (acc: List[EdgeType], n :NodeType) =>
        n.wrongUsers.map{AGEdge.uses(_, n)} :::(
          if(n.isWronglyContained )
            AGEdge.contains(n.container, n) :: acc
          else acc)
    }
  }


  def discardConstraints() {
    nodeSets.clear()
    this.foreach(_.discardConstraints())
  }

  def printConstraints[V](logger : Logger[V]){
    nodeSets.foreach{
      case (_, namedSet) => logger.writeln(namedSet.defString)
    }
    constraints.foreach(ct => logger.writeln(ct.toString))
  }

  def printUsesDependancies[V](logger : Logger[V]){
    this.foreach { node =>
      if (node.primaryUses.nonEmpty)
        logger.writeln(node.primaryUses.toString())
      if (node.sideUses.nonEmpty)
        logger.writeln(node.sideUses.toString())
    }
  }

  private [puck] val nodesByName = mutable.Map[String, NodeType]()

  def apply(fullName:String) : NodeType= nodesByName(fullName)
  def getNode(fullName:String) : Option[NodeType] = nodesByName get fullName


  def addNode(fullName: String, localName:String, kind: Kind): NodeType = {
    val unambiguousFullName = nodeBuilder.makeKey(fullName, localName, kind)
    nodesByName get unambiguousFullName match {
      case None =>
        val n = addNode(localName, kind)
        this.nodesByName += (unambiguousFullName -> n)
        n
      case Some(n) => n /* check that the kind and type is indeed the same ??*/
    }
  }

  def remove(n : NodeType){
    nodes0 -= n
    transformations.removeNode(n)
  }

  var transformations : CareTaker[Kind] = new CareTaker(this)
  var initialRecord : List[Transformation[Kind]] = _

  def apply(r : Recording[Kind]){
    transformations.recording.undo()
    transformations.recording = r
  }


  def addNode(n : NodeType) : NodeType = {
    //assert n.graph == this ?
    this.nodes0 += n
    transformations.addNode(n)
    n
  }

  def addNode(localName:String, kind: Kind) : NodeType = {
    id = id + 1
    val n = nodeBuilder(this, id, localName, kind.create())
    addNode(n)
    //this.root.content_+=(n)
    //n
  }

  def addUsesDependency(primary : EdgeType, side : EdgeType){
    primary.user.sideUses += (primary.usee, side)
    side.user.primaryUses += (side.usee, primary)
    transformations.addEdgeDependency(primary, side)
  }
  def addUsesDependency(primaryUser : NodeType, primaryUsee : NodeType,
                        sideUser : NodeType, sideUsee : NodeType) {
    addUsesDependency(AGEdge.uses(primaryUser, primaryUsee),
      AGEdge.uses(sideUser, sideUsee))
  }

  def removeUsesDependency(primary : EdgeType, side : EdgeType){
    primary.user.sideUses -= (primary.usee, side)
    side.user.primaryUses -= (side.usee, primary)
    transformations.removeEdgeDependency(primary, side)
  }

  def removeUsesDependency(primaryUser : NodeType, primaryUsee : NodeType,
                           sideUser : NodeType, sideUsee : NodeType) {
    removeUsesDependency(AGEdge.uses(primaryUser, primaryUsee),
      AGEdge.uses(sideUser, sideUsee))
  }


  def applyChangeOnProgram(){}

  def coupling = this.foldLeft(0 : Double){ (acc, n) => acc + n.coupling }

}

