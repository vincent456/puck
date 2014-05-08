package puck.graph

import scala.collection.mutable

/**
 * Created by lorilan on 05/05/14.
 */

abstract class NodeKind
case class AGRoot private[graph]() extends NodeKind

class AGNode (val graph: AccessGraph,
              val id: Int,
              var name: String,
              val kind: NodeKind,
              var `type`: Option[Type]){

  def ==(that:AGNode):Boolean = this.id == that.id

  private var container : Option[AGNode] = None

  private[graph] val getContainer = container
  private[graph] def setContainer(n: AGNode){
    container = Some(n)
    n.addContent(this)
  }
  //private[graph] def setContainer(container:Option[Int]) { this.container = container}

  private var content : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addContent(n:AGNode) {
    this.content += n
    n.container = Some(this)
  }
  def getContent : mutable.Iterable[AGNode] = content
  def isContentEmpty = content.isEmpty


  private var superTypes : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addSuperType(st:AGNode) {
    this.superTypes += st
    st.subTypes += this
  }
  private[graph] def getSuperTypes:Iterable[AGNode] = superTypes

  private var subTypes : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addSubType(st:AGNode) {
    this.subTypes += st
    st.superTypes += this
  }

  private var users : mutable.Set[AGNode] = mutable.HashSet()
  private[graph] def addUser(n:AGNode) { this.users += n}
  private[graph] def getUsers: mutable.Iterable[AGNode] = users

  private var primaryUses : Set[Int] = Set()
  private var sideUses : Set[Int] = Set()


  def nameTypeString = name + ( `type` match{ case None =>""; case Some(t) => " : " + t })

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