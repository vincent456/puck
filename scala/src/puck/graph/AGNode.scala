package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */

import scala.collection.mutable

object NodeKind extends Enumeration{
  val Package = Value //unused in core LJ
  val Interface = Value //unused in LJ

  val Class = Value
  val Constructor = Value
  val Method = Value
}

class AGNode (val graph: AccessGraph,
              val id: Int,
              var name: String,
              val kind: NodeKind.Value,
              var `type`: Option[Type]){

  def ==(that:AGNode):Boolean = this.id == that.id

  private var container : Option[Int] = None

  private[graph] def setContainer(container:Option[Int]) { this.container = container}

  private var content : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addContent(n:AGNode) { this.content += n}


  private var superTypes : Set[Int] = Set()
  private[graph] def addSuperType(st:Int) { this.superTypes = this.superTypes + st}

  private var subTypes : Set[Int] = Set()
  private[graph] def addSubType(st:Int) { this.subTypes = this.subTypes + st}

  private var users : Set[Int] = Set()
  private[graph] def addUser(u:Int) { this.users = this.users + u}

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