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

  private var container0 : Option[AGNode] = None
  def container = container0
  def container_=(sn: Option[AGNode]){
    container0 = sn
    sn match {
      case Some(n) => n.content0 += this
      case None => ()
    }
  }

  private var content0 : mutable.Set[AGNode] = mutable.HashSet()

  def content : mutable.Iterable[AGNode] = content0
  def content_+=(n:AGNode) {
    this.content0 += n
    n.container0 = Some(this)
  }

  private[graph] def isContentEmpty = content0.isEmpty


  private var superTypes0 : mutable.Set[AGNode] = mutable.HashSet()

  def superTypes:Iterable[AGNode] = superTypes0
  def superTypes_+=(st:AGNode) {
    this.superTypes0 += st
    st.subTypes0 += this
  }

  private var subTypes0 : mutable.Set[AGNode] = mutable.HashSet()
  def subTypes_+=(st:AGNode) {
    this.subTypes0 += st
    st.superTypes0 += this
  }

  private var users0 : mutable.Set[AGNode] = mutable.HashSet()
  def users_+=(n:AGNode) { this.users0 += n}
  def users: mutable.Iterable[AGNode] = users0

  private var primaryUses : Set[Int] = Set()
  private var sideUses : Set[Int] = Set()


  override def toString: String = nameTypeString

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