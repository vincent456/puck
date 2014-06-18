package puck.graph

import scala.collection.mutable
import puck.graph.constraints.{Constraint, NamedNodeSet}

/**
 * Created by lorilan on 05/05/14.
 */

object AccessGraph {
  val rootId = 0
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"
}

class AccessGraph (nodeBuilder : AGNodeBuilder) {
  //extends Iterable[AGNode]{ //is NOT iterable (see scala doc for requirements) but
  // has a valid iterator + implicit conversion in puck.graph package object

  println("Node builder : " + nodeBuilder.getClass)

  val nodeSets : mutable.Map[String, NamedNodeSet] = mutable.Map()
  val constraints : mutable.Buffer[Constraint] = mutable.Buffer()

  private [this] val nodes0 = mutable.Buffer[AGNode]()
  def nodes : Iterator[AGNode] = nodes0.iterator

  /*private[graph] val predefTypes : mutable.Map[String, Type] = mutable.Map()
  def predefType(name : String ) = predefTypes(name)*/

  private var id : Int = 1
  val root : AGNode = nodeBuilder(this, AccessGraph.rootId, AccessGraph.rootName, AGRoot())

  val scopeSeparator = nodeBuilder.scopeSeparator

  /*  def attachRoots() {
      this.foreach{ n =>
        if(n.container == n && n != root){
             root content_+= n
        }
      }
    }*/

  def nodeKinds = nodeBuilder.kinds

  def iterator = root.iterator

  def violations : List[AGEdge] = {
    this.foldLeft(List[AGEdge]()){
      (acc: List[AGEdge], n :AGNode) =>
        n.wrongUsers.map{AGEdge.uses(_, n)} :::(
          if(n.isWronglyContained )
            AGEdge.contains(n.container, n) :: acc
          else acc)
    }
  }

  def discardConstraints() {
    this.foreach(_.discardConstraints())
  }

  def printConstraints(){
    nodeSets.foreach{
      case (_, namedSet) => println(namedSet.defString)
    }
    constraints.foreach(ct => println(ct))
  }

  def printUsesDependancies(){
    this.foreach { node =>
      if (!node.primaryUses.isEmpty)
        println(node.primaryUses)
      if (!node.sideUses.isEmpty)
        println(node.sideUses)
    }
  }

  def list(){
    println("AG nodes :")
    nodes0.foreach(n => println("- " + n))
    println("list end")
  }

  private [puck] val nodesByName = mutable.Map[String, AGNode]()

  def apply(fullName:String) : AGNode= nodesByName(fullName)
  def getNode(fullName:String) : Option[AGNode] = nodesByName get fullName

  def addNode(fullName: String, localName:String, kind: NodeKind): AGNode = {
    val unambiguousFullName = nodeBuilder.makeKey(fullName, localName, kind)
    nodesByName get unambiguousFullName match {
      case None =>
        val n = addNode(localName, kind)
        this.nodesByName += (unambiguousFullName -> n)
        n
      case Some(n) => n /* check that the kind and type is indeed the same ??*/
    }
  }

  def addNode(fullName: String, localName:String): AGNode =
    addNode(fullName, localName, VanillaKind())

  def remove(n : AGNode){
    nodes0 -= n
    transformations.removeNode(n)
  }

  var transformations : CareTaker = new CareTakerNoop(this)

  def register[T](op : => T) : T = {
    transformations.startRegister()
    val res = transformations.sequence[T](op)
    transformations.stopRegister()
    res
  }

  def addNode(n : AGNode) : AGNode = {
    //assert n.graph == this ?
    this.nodes0 += n
    transformations.addNode(n)
    n
  }

  def addNode(localName:String, kind: NodeKind) : AGNode = {
    id = id + 1
    val n = nodeBuilder(this, id, localName, kind)
    addNode(n)
    //this.root.content_+=(n)
    //n
  }

  def addNode(localName : String) : AGNode =
    addNode(localName, VanillaKind())

  def addUsesDependency(primary : AGEdge, side : AGEdge){
    primary.user.sideUses += (primary.usee, side)
    side.user.primaryUses += (side.usee, primary)
    transformations.addEdgeDependency(primary, side)
  }
  def addUsesDependency(primaryUser : AGNode, primaryUsee : AGNode,
                        sideUser : AGNode, sideUsee : AGNode) {
    addUsesDependency(AGEdge.uses(primaryUser, primaryUsee),
      AGEdge.uses(sideUser, sideUsee))
  }

  def removeUsesDependency(primary : AGEdge, side : AGEdge){
    primary.user.sideUses -= (primary.usee, side)
    side.user.primaryUses -= (side.usee, primary)
    transformations.removeEdgeDependency(primary, side)
  }
  def removeUsesDependency(primaryUser : AGNode, primaryUsee : AGNode,
                           sideUser : AGNode, sideUsee : AGNode) {
    removeUsesDependency(AGEdge.uses(primaryUser, primaryUsee),
      AGEdge.uses(sideUser, sideUsee))
  }

}

