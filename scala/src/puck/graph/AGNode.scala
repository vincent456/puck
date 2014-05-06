package puck.graph

import scala.collection.mutable

/**
 * Created by lorilan on 05/05/14.
 */

class NodeKind

class AGNode (val graph: AccessGraph,
              val id: Int,
              var name: String,
              val kind: NodeKind,
              var `type`: Option[Type]){

  def ==(that:AGNode):Boolean = this.id == that.id

  private var container : Option[Int] = None

  private[graph] def setContainer(container:Option[Int]) { this.container = container}

  private var content : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addContent(n:AGNode) { this.content += n}


  private var superTypes : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addSuperType(st:AGNode) { this.superTypes += st}

  private var subTypes : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addSubType(st:AGNode) { this.subTypes += st}

  private var users : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addUser(n:AGNode) { this.users += n}

  private var primaryUses : Set[Int] = Set()
  private var sideUses : Set[Int] = Set()

  //constraints
  /**
   * Friends, Interlopers and Facade are scopes.
   */
  /**
   * friends bypass other constraints
   */
  private var friends : List[Int] = List()
  /**
   * this scope is hidden
   * 3-tuple (Constraint - id, Interlopers id, Friends Id)
   */
  private var scopeInterlopers: List[(Int, List[Int], List[Int])]= List()
  /**
   * pairs of (nodeId, constraintId)
   * this is a facade for the scope rooted by nodId for the constraint constraintId
   */
  private var facadeOf:List[(Int, Int)] = List()
  /**
   * this element is hidden but not the elements that it contains
   * 2-tuple (Interlopers id, Friends Id)
   */
  private var elementInterlopers : List[(List[Int], List[Int])]= List()


}