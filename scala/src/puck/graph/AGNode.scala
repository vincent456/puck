package puck.graph

import scala.collection.mutable
import scala.language.implicitConversions
import AGNode.toScopeSet
import scala.annotation.tailrec


trait AGNodeBuilder {
  def apply(g: AccessGraph, id: Int, name:String, kind : NodeKind) : AGNode
  def makeKey(fullName: String, localName:String, kind: NodeKind) : String
}

object AGNode extends AGNodeBuilder{
  override def apply(g: AccessGraph,
                     id: Int, name : String,
                     kind : NodeKind) : AGNode = new AGNode(g, id, name, kind)


  override def makeKey(fullName: String, localName:String,
                       kind : NodeKind) : String = fullName


  implicit def toScopeSet( n: mutable.Set[AGNode]) : ScopeSet = new ScopeSet(n.iterator)
  implicit def toScopeSet( l: List[AGNode]) : ScopeSet = new ScopeSet(l.iterator)


  def addUsesDependency(primaryUser : AGNode, primaryUsee : AGNode,
                        sideUser : AGNode, sideUsee : AGNode) {
    primaryUser.sideUses get primaryUsee match {
      case None =>
        primaryUser.sideUses += (primaryUsee -> mutable.Set(AGEdge.uses(sideUser, sideUsee)))
      case Some(s)=>
        primaryUser.sideUses += (primaryUsee -> s.+=(AGEdge.uses(sideUser, sideUsee)))

    }

    sideUser.primaryUses += (sideUsee -> AGEdge.uses(primaryUser, primaryUsee))
  }
}

class ScopeSet(it : Iterator[AGNode]){
  def scopeThatContains_*(elem: AGNode) = it.find { _.contains_*(elem) }
  def hasScopeThatContains_*(elem: AGNode) = it.exists { _.contains_*(elem) }
}

class AGNode (val graph: AccessGraph,
              val id: Int,
              var name: String,
              val kind: NodeKind) { //extends Iterable[AGNode]{



  /*override def equals(obj:Any) : Boolean = obj match {
    case that : AGNode => this.id == that.id
    case _ => false
  }
  override def hashCode : Int = this.id*/


  /**
   * relies on the contains tree : do not modify it while traversing
   */
  def iterator = new AGNodeIterator(this)

  override def toString: String = name

  //TODO FIX def
  def nameTypeString = name //+ ( `type` match{ case None =>""; case Some(t) => " : " + t })


  def fullName : String = container match{
    case None => nameTypeString
    case Some(p) => if(p == graph.root) nameTypeString
    else p.fullName + "." + nameTypeString
  }

  /**
   * Arcs
   */

  private var container0 : Option[AGNode] = None
  def container = container0
  def container_=(sn: Option[AGNode]){
    container0 = sn
    sn match {
      case Some(n) => n.content0 += this
      case None => ()
    }
  }

  def container_! = container0 match {
    case None => throw new AGError(this + " does not have a container")
    case Some(c) => c
  }

  def canContain(k : NodeKind) = true

  private var content0 : mutable.Set[AGNode] = mutable.Set()

  def content : mutable.Iterable[AGNode] = content0
  def content_+=(n:AGNode) {
    this.content0 += n
    n.container0 = Some(this)
  }

  private[graph] def isContentEmpty = content0.isEmpty

  def contains(other :AGNode) = content0.contains(other)

  def contains_*(other:AGNode) : Boolean =
    other == this ||
      (other.container match {
        case None => false
        case Some(p) => this.contains_*(p)
      })


  private var superTypes0 : mutable.Set[AGNode] = mutable.Set()

  def `is super type of`(other : AGNode) : Boolean = {
    subTypes0.contains(other) ||
    subTypes0.exists(_.`is super type of`(other))
  }

  def superTypes:Iterable[AGNode] = superTypes0
  def superTypes_+=(st:AGNode) {
    this.superTypes0 += st
    st.subTypes0 += this
  }

  private var subTypes0 : mutable.Set[AGNode] = mutable.Set()
  def subTypes_+=(st:AGNode) {
    this.subTypes0 += st
    st.superTypes0 += this
  }

  private var uses0 : mutable.Set[AGNode] = mutable.Set()

  def uses(other : AGNode) = uses0.contains(other)

  private var users0 : mutable.Set[AGNode] = mutable.Set()
  def users_+=(other : AGNode) {
    this.users0 += other
    other.uses0 += this
  }

  def removeUser(user : AGNode){
    users0.remove(user)
    user.uses0.remove(this)
  }

  def users: mutable.Iterable[AGNode] = users0

  def unsuscribeSideUses(sideUsee : AGNode){
    primaryUses.get(sideUsee) match {
      case None => ()
      case Some(primary_uses) =>
        val primUser = primary_uses.source
        val sec_uses = primUser.sideUses(primary_uses.target)
        sec_uses.remove(AGEdge.uses(this, sideUsee))
        if(sec_uses.isEmpty){
          primUser.sideUses.remove(primary_uses.target)
        }
    }
  }

  def redirectSideUses(primaryUsee: AGNode, newUsee : AGNode){
    sideUses.get(primaryUsee) match {
      case None => ()
      case Some( sides_uses ) =>
        val new_side_uses = sides_uses map {(edge : AGEdge) =>
          val side_usee_opt =
              edge.target.abstractions.find { (abs: AGNode) =>
                newUsee.contains_*(abs)
              }
          side_usee_opt match {
            case None => throw new AGError("No satisfying abstraction to redirect side use !")
            case Some( new_side_usee ) => AGEdge.uses(edge.source, new_side_usee)
          }

        }
        sideUses.remove(primaryUsee)
        sideUses.get(newUsee) match {
          case None => sideUses.put(primaryUsee, new_side_uses)
          case Some(oldSideUses) => oldSideUses ++= new_side_uses
        }
    }
  }

  def redirectUses(oldUsee : AGNode, newUsee : AGNode){
    oldUsee removeUser this
    newUsee users_+= this

    unsuscribeSideUses(oldUsee)
    redirectSideUses(oldUsee, newUsee)
  }

  /* a primary use is for example the use of a class when declaring a variable or a parameter
     a side use is in this example a call to a method with the same variable/parameter
        the use of the method is a side use of the declaration
         a couple primary use/ side use is a use dependency
   */

  /*(this, key) is a primary uses and sidesUses(key) are the corresponding side uses */
  private var sideUses : mutable.Map[AGNode, mutable.Set[AGEdge]] = mutable.Map()



  /*(this, key) is a side uses and primaryUses(key) is the corresponding primary use */
  private var primaryUses : mutable.Map[AGNode, AGEdge] = mutable.Map()






  /**********************************************/
  /************** Constraints********************/
  /**********************************************/
  /**
   * Friends, Interlopers and Facade are scopes.
   */
  /**
   * friends bypass other constraints
   */
  private var friendsSet : mutable.Set[AGNode] = mutable.Set()

  def friends : mutable.Iterable[AGNode] = friendsSet
  def friends_++= = friendsSet ++= _

  /**
   * this scope is hidden
   */
  private var scopeConstraints: mutable.Buffer[ScopeConstraint]= mutable.Buffer()

  def scopeConstraints_+=(ct : ScopeConstraint) = scopeConstraints += ct

  /**
   * this element is hidden but not the elements that it contains
   */
  private var elementConstraints : mutable.Buffer[ElementConstraint]= mutable.Buffer()

  def elementConstraints_+=(x : ElementConstraint) = elementConstraints += x

  /**
   * Constraints Handling
   */

  def discardConstraints() {
    friendsSet = mutable.Set()
    scopeConstraints = mutable.Buffer()
    elementConstraints = mutable.Buffer()
  }

  def constraintsString : String =
    if(!(friendsSet.isEmpty && elementConstraints.isEmpty && scopeConstraints.isEmpty )){
      this.fullName + ":\n" +
        (if(!friendsSet.isEmpty)
          "friends([" + friendsSet.mkString(", ") +"]).\n"
        else "")+
        scopeConstraints.mkString("\n") +
        elementConstraints.mkString("\n") +"\n****************************************"}
    else ""


  @tailrec
  final def friendOf(other : AGNode) : Boolean = other.friendsSet.hasScopeThatContains_*( this ) ||
    (other.container match {
      case None => false
      case Some(p) => friendOf(p)
    })


  /*
      hiddenFrom(Element, Interloper) :- hideScope(S, Facades, Interlopers, Friends),
          friends(S,Friends,AllFriends),
          'vContains*'(S,Element),			% Element is in S
          \+ 'gContains*'(Facades,Element),		% Element is not in one of the Facades
          'gContains*'(Interlopers,Interloper),	% Interloper is in one of the Interlopers
          \+ 'gContains*'(AllFriends, Interloper),	% but not in one the Friends
          \+ 'vContains*'(S,Interloper).		% Interloper is not in S
  */

  def potentialScopeInterloperOf(other0 : AGNode) : Boolean = {
   @tailrec
    def aux(other: AGNode): Boolean =
     !other.contains_*(this) &&
     other.scopeConstraints.exists { (ct) =>
        ct.interlopers.hasScopeThatContains_*(this) &&
          !(ct.friends.hasScopeThatContains_*(this) ||
            ct.facades.hasScopeThatContains_*(other0))
      } || (other.container match {
        case None => false
        case Some(p) => aux(p)
      })
    aux(other0)

  }

  /*
      hiddenFrom(Element, Interloper) :- hide(Element, Interlopers, Friends),
           friends(Element, Friends, AllFriends),
           'gContains*'(Interlopers, Interloper),
           \+ 'gContains*'(AllFriends, Interloper).

  */

  def potentialElementInterloperOf(other:AGNode) =
    other.elementConstraints.exists{ (ct) =>
      ct.interlopers.hasScopeThatContains_*(this) &&
        !ct.friends.hasScopeThatContains_*(this)
    }

  def interloperOf(other : AGNode) =
    (potentialScopeInterloperOf(other)
      || potentialElementInterloperOf(other)) && !friendOf(other)

  def isWronglyContained : Boolean = {
    container match {
      case None => false
      case Some(c) => c interloperOf this
    }
  }

  def wrongUsers : List[AGNode] = {
    users.foldLeft(List[AGNode]()){(acc:List[AGNode], user:AGNode) =>
      if( user interloperOf this ) user :: acc
      else acc
    }
  }

  /**
   * Solving
   */

  private var abstractions0: mutable.Set[AGNode]= mutable.Set()
  def abstractions : mutable.Iterable[AGNode] = abstractions0

  def `may be an abstraction of`(other : AGNode) = false

  def searchExistingAbstractions(){
    graph.iterator foreach { ( n : AGNode) =>
      if(n != this && (n `may be an abstraction of` this))
        this.abstractions0 += n
    }
  }

  def isUserOfItsAbstractionKind = true

  def createNodeAbstraction() : AGNode = {
    // TODO find a strategy or way to make the user choose which abstractkind is used !
    val n = graph.addNode(name + "_abstraction", kind.abstractKinds.head)
    abstractions0 += n
    /* little hack (?) : an abstraction is its own abstraction otherwise,
     on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ...
     and that can go on... */
    n.abstractions0 += n
    n
  }

  def createAbstraction() : AGNode = {
      val abs = createNodeAbstraction()
      users_+=(abs)
      abs
  }

}



