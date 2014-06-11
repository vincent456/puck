package puck.graph

import scala.collection.mutable
import puck.graph.constraints.{Constraint, NamedNodeSet}

/**
 * Created by lorilan on 05/05/14.
 */

object AccessGraph {
  val rootId = 0
}

class AccessGraph (nodeBuilder : AGNodeBuilder) {
//extends Iterable[AGNode]{ //is NOT iterable (see scala doc for requirements) but
// has a valid iterator + implicit conversion in puck.graph package object

  println("Node builder : " + nodeBuilder.getClass)

  val nodeSets : mutable.Map[String, NamedNodeSet] = mutable.Map()
  val constraints : mutable.Buffer[Constraint] = mutable.Buffer()

  val nodes = mutable.Buffer[AGNode]()
  /*private[graph] val predefTypes : mutable.Map[String, Type] = mutable.Map()
  def predefType(name : String ) = predefTypes(name)*/

  private var id : Int = 1
  val root : AGNode = nodeBuilder(this, AccessGraph.rootId, "root", AGRoot())

  def nodeKinds = nodeBuilder.kinds

  def iterator = root.iterator

  def violations : List[AGEdge] = {
     this.foldLeft(List[AGEdge]()){
      (acc: List[AGEdge], n :AGNode) =>
      n.wrongUsers.map{AGEdge.uses(_, n)} :::(
        if(n.isWronglyContained )
          AGEdge.contains(n.container_!, n) :: acc
        else acc)
    }
  }

  /*def violations : Set[Violation] = {
    this.foldLeft(Set[Violation]()){(acc: Set[Violation], n :AGNode) => n.targetingViolations(acc)}
  }*/

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
    nodes.foreach(println)
  }

  def attachNodesWithoutContainer() {
    nodes.foreach{ n =>
      n.container match {
        case None => root content_+= n
        case Some(_) => ()
      }

    }
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


  var careTaker : CareTaker = new CareTakerNoop()

  def registerModifications() {
      careTaker = new CareTakerRegister()
  }

  def stopModificationsRegistrations(){
    careTaker = new CareTakerNoop()
  }

  def addNode(localName:String, kind: NodeKind) : AGNode = {
    id = id + 1
    val n = nodeBuilder(this, id, localName, kind)
    this.nodes += n
    n
  }

  def addUsesDependency(primaryUser : AGNode, primaryUsee : AGNode,
                        sideUser : AGNode, sideUsee : AGNode) {
    primaryUser.sideUses += (primaryUsee, AGEdge.uses(sideUser, sideUsee))
    sideUser.primaryUses += (sideUsee, AGEdge.uses(primaryUser, primaryUsee))
  }

  def removeUsesDependency(primaryUser : AGNode, primaryUsee : AGNode,
                           sideUser : AGNode, sideUsee : AGNode) {
    primaryUser.sideUses -= (primaryUsee, AGEdge.uses(sideUser, sideUsee))
    sideUser.primaryUses -= (sideUsee, AGEdge.uses(primaryUser, primaryUsee))
  }

}

