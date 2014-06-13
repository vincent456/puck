package puck.graph

import scala.collection.mutable
import scala.annotation.tailrec
import puck.graph.constraints._
import scala.Some


trait AGNodeBuilder {
  def apply(g: AccessGraph, id: Int, name:String, kind : NodeKind) : AGNode
  def makeKey(fullName: String, localName:String, kind: NodeKind) : String
  def kinds : List[NodeKind]
}

object AGNode extends AGNodeBuilder{
  override def apply(g: AccessGraph,
                     id: Int, name : String,
                     kind : NodeKind) : AGNode = new AGNode(g, id, name, kind)


  override def makeKey(fullName: String, localName:String,
                       kind : NodeKind) : String = fullName



  val kinds = List(VanillaKind())

  def implUsesAbs(implKind : NodeKind, absKind : NodeKind) : Boolean =
    (implKind, absKind) match {
      case (VanillaKind(), VanillaKind()) => false
      case _ => throw new AGError("do not know if impl (%s) uses abs (%s)".format(implKind, absKind))
    }
}

class AGNode (val graph: AccessGraph,
              val id: Int,
              var name: String,
              val kind: NodeKind) { //extends Iterable[AGNode]{

  override def equals(obj:Any) : Boolean = obj match {
    case that : AGNode => (this.graph eq that.graph) && this.id == that.id
    case _ => false
  }
  override def hashCode : Int = (this.id  + kind.hashCode()) / 41

  /**
   * relies on the contains tree : do not modify it while traversing
   */
  def iterator = new AGNodeIterator(this)

  override def toString: String = "(" + fullName +", "+ kind+ ")"

  //TODO FIX def
  def nameTypeString = name //+ ( `type` match{ case None =>""; case Some(t) => " : " + t })


  def fullName : String =
    if (isRoot) nameTypeString
    else containerPath(graph.root).tail.map(_.nameTypeString).mkString(".")

  /**
   * Arcs
   */

  private var container0 : AGNode = this

  def container = container0

  def isRoot = container == this

  /* !!!! unregistred operation use only to undo node creation !!!! */
  def detach(){
    container0.content0 -= this
    container0 = this
  }

  def canContain(n : AGNode) : Boolean = {
    this.kind canContain n.kind
  }

  private var content0 : mutable.Set[AGNode] = mutable.Set()

  def content : mutable.Iterable[AGNode] = content0
  def content_+=(n:AGNode) {
    n.container0.content0 -= n
    n.container0 = this
    this.content0 += n
    graph.transformations.addEdge(AGEdge.contains(this, n))
  }

  def content_-=(n:AGNode) {
    n.container0 = n
    this.content0 -= n
    graph.transformations.removeEdge(AGEdge.contains(this, n))
  }

  private[graph] def isContentEmpty = content0.isEmpty

  def contains(other :AGNode) = content0.contains(other)

  def contains_*(other:AGNode) : Boolean =
    other == this || !other.isRoot && this.contains_*(other.container)


  private def containerPath_aux(ancestor : AGNode, acc : List[AGNode]) : List[AGNode] =
    if(ancestor == this) this :: acc
    else if(isRoot) throw new AGError("container path, ancestor not found !")
    else container.containerPath_aux(ancestor, this :: acc)

  def containerPath(ancestor : AGNode) : List[AGNode] = containerPath_aux(ancestor, List())


  private var superTypes0 : mutable.Set[AGNode] = mutable.Set()

  def isSuperTypeOf(other : AGNode) : Boolean = {
    other.superTypes.exists(_ == this) ||
      other.superTypes.exists(_.isSuperTypeOf(this))
  }


  def superTypes : Iterable[AGNode] = superTypes0
  def superTypes_+=(st:AGNode) {
    this.superTypes0 += st
    graph.transformations.addEdge(AGEdge.isa(this, st))
    //st.subTypes0 += this
  }

  def superTypes_-=(st:AGNode) {
    this.superTypes0 -= st
    graph.transformations.removeEdge(AGEdge.isa(this, st))

    //st.subTypes0 -= this
  }

  /*def isSuperTypeOf(other : AGNode) : Boolean = {
    subTypes0.contains(other) ||
      subTypes0.exists(_.isSuperTypeOf(other))
  }
  private var subTypes0 : mutable.Set[AGNode] = mutable.Set()
  def subTypes_+=(st:AGNode) {
    this.subTypes0 += st
    st.superTypes0 += this
  }*/

  //TODO think about removing uses0 & users0 and keep only the sideUses0/primaryUses0 maps
  //private var uses0 : mutable.Set[AGNode] = mutable.Set()
  def uses(other : AGNode) = other.isUsedBy(this)// uses0.contains(other)
  def uses_+=(other: AGNode) = other.users_+=(this)
  def uses_-=(other: AGNode) = other.users_-=(this)

  private [this] var users0 : mutable.Set[AGNode] = mutable.Set()
  def users_+=(other : AGNode) {
    this.users0 += other
    graph.transformations.addEdge(AGEdge.uses(other, this))
  }
  def users_-=(user : AGNode){
    users0.remove(user)
    graph.transformations.removeEdge(AGEdge.uses(user, this))
  }

  def users: mutable.Iterable[AGNode] = users0

  def isUsedBy(other : AGNode) = users0.contains(other)

  /* a primary use is for example the use of a class when declaring a variable or a parameter
   a side use is in this example a call to a method with the same variable/parameter
      the use of the method is a side use of the declaration
       a couple primary use/ side use is a use dependency
 */

  /*(this, key) is a primary uses and sidesUses(key) are the corresponding side uses */
  val sideUses = new UsesDependencyMap(this, Dominant())

  /*(this, key) is a side uses and primaryUses(key) is the corresponding primary uses */
  val primaryUses = new UsesDependencyMap(this, Dominated())

  def findNewPrimaryUsee(primaryUsee : AGNode,
                         newSideUsee : AGNode,
                         policy : RedirectionPolicy) : AGNode = {
    policy match {
      case Move() => newSideUsee.container
      case absPolicy : AbstractionPolicy =>
        primaryUsee.abstractions.find{
          case (node, `absPolicy`) => node.contains_*(newSideUsee)
          case _ => false
        } match {
          case Some((n, _)) => n
          case None =>
            graph.iterator.find{ node =>
              node.contains_*(newSideUsee) &&
                primaryUsee.kind.abstractKinds(absPolicy).contains(node.kind)
            } match {
              case Some(n) => n
              case None => throw new AGError("no correct primary abstraction found !")
            }
        }
    }
  }

  def redirectPrimaryUses(sideUsee : AGNode,
                          newUsee : AGNode,
                          policy : RedirectionPolicy){
    primaryUses(sideUsee) match {
      case None => ()
      case Some(primary_uses) =>
        if(primary_uses.isEmpty){
          throw new AGError("WTF? empty primary uses set")
        }
        val primary_use = primary_uses.head
        if(primary_uses.tail.nonEmpty) {
          println("redirecting side uses : (" + this + ", " + sideUsee
            + ") to (" + this + ", " + newUsee + ")")
          println("primary uses are ")
          println( primary_use)
          primary_uses.tail.mkString("\n")
          throw new AGError("Do not know how to unsuscribe a side use with multiple primary uses")
        }
        else {

          val primUser = primary_use.source
          primUser.sideUses -= (primary_use.target, AGEdge.uses(this, sideUsee))
          primaryUses -= sideUsee

          val newPrimUsee = findNewPrimaryUsee(primary_use.target,
            newUsee, policy)

          primUser.sideUses += (newPrimUsee, AGEdge.uses(this, newUsee))
          primaryUses += (newUsee, AGEdge.uses(primUser, newPrimUsee))
        }
    }
  }

  def redirectSideUses(primaryUsee: AGNode,
                       newUsee : AGNode,
                       policy : RedirectionPolicy){
    sideUses(primaryUsee) match {
      case None => ()
      case Some( sides_uses ) =>
        val new_side_uses = mutable.Set[AGEdge]()
        sides_uses foreach {
          edge =>
            val side_usee_opt =
              edge.target.abstractions.find {
                case (abs, `policy`) => newUsee.contains_*(abs)
                case _ => false
              }
            side_usee_opt match {
              case None => throw new AGError("While redirecting primary uses (" + this +", " + primaryUsee +") to ("
                + this + ", " + newUsee
                + ")\nno satisfying abstraction to redirect side use "+ edge)
              case Some( (new_side_usee, _) ) =>
                new_side_uses += AGEdge.uses(edge.source, new_side_usee)
            }
        }

        sideUses -= primaryUsee

        sideUses ++= (newUsee, new_side_uses)
    }
  }

  def redirectUses(oldUsee : AGNode, newUsee : AGNode,
                   policy : RedirectionPolicy){
    oldUsee users_-= this
    newUsee users_+= this

    redirectPrimaryUses(oldUsee, newUsee, policy)
    redirectSideUses(oldUsee, newUsee, policy)
  }


  def createContainer() : AGNode = {
    //TODO user choice ? better strategy ?
    graph.nodeKinds.find(_.canContain(this.kind)) match {
      case None => throw new AGError("do not know how to create a valid parent for " + this.kind)
      case Some(parentKind) =>
        val n = graph.addNode (this.name + "_container", parentKind)
        n content_+= this
        n
    }
  }


  def moveTo(newContainer : AGNode) {
    container.content_-=(this)
    newContainer content_+= this
    users.foreach{
      _.redirectPrimaryUses(this, this, Move())
    }
  }

  /**********************************************/
  /************** Constraints********************/
  /**********************************************/
  /**
   * Friends, Interlopers and Facade are scopes.
   */

  /**
   * friends bypass other constraints
   */
  val friendConstraints = new ConstraintSet[FriendConstraint]()
  /*
   * assert owners contains this
   */
  /**
   * this scope is hidden
   */
  val scopeConstraints = new ConstraintSet[ScopeConstraint]()

  /**
   * this element is hidden but not the elements that it contains
   */
  val elementConstraints = new ConstraintSet[ElementConstraint]()

  /**
   * Constraints Handling
   */

  def discardConstraints() {
    friendConstraints.clear()
    scopeConstraints.clear()
    elementConstraints.clear()
  }

  def remove(ct : Constraint) = ct match {
    case fct @ FriendConstraint(_,_) => friendConstraints -= fct
    case ect @ ElementConstraint(_,_,_) => elementConstraints -= ect
    case sct @ ScopeConstraint(_,_,_,_) => scopeConstraints -= sct
  }

  final def friendOf(other : AGNode) : Boolean = other.friendConstraints.hasFriendScopeThatContains_*( this ) ||
    !other.isRoot && friendOf(other.container)

  def violatedScopeConstraintsOf(usee0 : AGNode) : List[ScopeConstraint] = {
    val uses = AGEdge.uses(this, usee0)

    def aux(usee : AGNode, acc : List[ScopeConstraint]) : List[ScopeConstraint] = {
      val acc2 = if(!usee.contains_*(this))
        usee.scopeConstraints.filter(_.violated(uses)).toList ::: acc
      else acc

      if(usee.isRoot) acc2
      else aux(usee.container, acc2)
    }
    aux(usee0,List())
  }


  def potentialScopeInterloperOf(usee0 : AGNode) : Boolean = {
    val uses = AGEdge.uses(this, usee0)

    def aux(usee: AGNode): Boolean =
      !usee.contains_*(this) &&
        usee.scopeConstraints.exists(_.violated(uses)) ||
        !usee.isRoot && aux(usee.container)

    aux(usee0)
  }

  def violatedElementConstraintOf(usee : AGNode) =
    usee.elementConstraints.filter(_.violated(AGEdge.uses(this, usee))).toList

  def potentialElementInterloperOf(usee:AGNode) =
    usee.elementConstraints.exists(_.violated(AGEdge.uses(this, usee)))

  def interloperOf(other : AGNode) =
    (potentialScopeInterloperOf(other)
      || potentialElementInterloperOf(other)) && !friendOf(other)

  def isWronglyContained : Boolean = !isRoot && (container interloperOf this)

  def wrongUsers : List[AGNode] = {
    users.foldLeft(List[AGNode]()){(acc:List[AGNode], user:AGNode) =>
      if( user interloperOf this ) user :: acc
      else acc
    }
  }

  /**
   * Solving
   */

  protected var abstractions0: mutable.Set[(AGNode, AbstractionPolicy)]= mutable.Set()
  def abstractions : mutable.Iterable[(AGNode, AbstractionPolicy)] = abstractions0
  def abstractions_-=(n : AGNode, p : AbstractionPolicy){
    abstractions0.remove( (n,p) )
    graph.transformations.unregisterAbstraction(this, n, p)

  }
  def abstractions_+= (n : AGNode, p : AbstractionPolicy){
    abstractions0 +=( (n,p) )
    graph.transformations.registerAbstraction(this, n, p)
  }
  def searchExistingAbstractions(){}


  def createNodeAbstraction(abskind :  NodeKind, policy : AbstractionPolicy) : AGNode = {
    // TODO find a strategy or way to make the user choose which abstractkind is used !
    val n = graph.addNode(name + "_abstraction", abskind)
    abstractions_+=(n, policy)
    /* little hack (?) : an abstraction is its own abstraction otherwise,
     on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ...
     and that can go on... */
    n.abstractions_+=(n, policy)
    n
  }

  def createAbstraction(abskind : NodeKind , policy : AbstractionPolicy) : AGNode = {
    val abs = createNodeAbstraction(abskind, policy)
    users_+=(abs)
    abs
  }

  def addHideFromRootException(friend : AGNode){
    def addExc(ct : ConstraintWithInterlopers) {
      if (ct.interlopers.iterator.contains(graph.root))
        ct.friends += friend
    }

    scopeConstraints foreach addExc
    elementConstraints foreach addExc
  }

}



