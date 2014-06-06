package puck.graph

import scala.collection.mutable
import scala.language.implicitConversions
import AGNode.toScopeSet
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


  implicit def toScopeSet( n: mutable.Set[AGNode]) : ScopeSet = new ScopeSet(n.iterator)
  implicit def toScopeSet( l: List[AGNode]) : ScopeSet = new ScopeSet(l.iterator)


  def addUsesDependency(primaryUser : AGNode, primaryUsee : AGNode,
                        sideUser : AGNode, sideUsee : AGNode) {
    primaryUser.sideUses_+=(primaryUsee, AGEdge.uses(sideUser, sideUsee))
    sideUser.primaryUses_+=(sideUsee, AGEdge.uses(primaryUser, primaryUsee))
  }

  val kinds = List(VanillaKind())

  def implUsesAbs(implKind : NodeKind, absKind : NodeKind) : Boolean =
    (implKind, absKind) match {
      case (VanillaKind(), VanillaKind()) => false
      case _ => throw new AGError("do not know if impl (%s) uses abs (%s)".format(implKind, absKind))
    }
}

class ScopeSet(it : Iterator[AGNode]){
  def scopeThatContains_*(elem: AGNode) = it.find { _.contains_*(elem) }
  def hasScopeThatContains_*(elem: AGNode) = it.exists { _.contains_*(elem) }

  def findCommonRoot : AGNode = {
    if(!it.hasNext)
      throw new AGError("empty node set")

    def aux(root : AGNode) : AGNode = {
      if(!it.hasNext)
        root
      else{
        val n = it.next()
        if(root.contains_*(n))
          aux(root)
        else if(n.contains_*(root))
          aux(n)
        else
          throw new AGError("no common root in ScopeSet")
      }
    }
    aux(it.next())
  }

}

class AGNode (val graph: AccessGraph,
              val id: Int,
              var name: String,
              val kind: NodeKind) { //extends Iterable[AGNode]{



  /*
  override def equals(obj:Any) : Boolean = obj match {
    case that : AGNode => this.graph == that.graph && this.id == that.id
    case _ => false
  }
  override def hashCode : Int = this.id
 */

  /**
   * relies on the contains tree : do not modify it while traversing
   */
  def iterator = new AGNodeIterator(this)

  override def toString: String = "(" + fullName +", "+ kind+ ")"

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

  def detach(){
    container0 match{
      case None => ()
      case Some(c) =>
        container0 = None
        c.content0.remove(this)
    }
  }

  def container_! = container0 match {
    case None => throw new AGError(this + " does not have a container")
    case Some(c) => c
  }

  def canContain(n : AGNode) : Boolean = {
    this.kind canContain n.kind
  }

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

  def isSuperTypeOf(other : AGNode) : Boolean = {
    subTypes0.contains(other) ||
      subTypes0.exists(_.isSuperTypeOf(other))
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

  //TODO think about removing uses0 & users0 and keep only the sideUses0/primaryUses0 maps
  //private var uses0 : mutable.Set[AGNode] = mutable.Set()
  def uses(other : AGNode) = other.isUsedBy(this)// uses0.contains(other)
  def uses_+=(other: AGNode) = other.users_+=(this)
  def uses_-=(other: AGNode) = other.users_-=(this)

  private [this] var users0 : mutable.Set[AGNode] = mutable.Set()
  def users_+=(other : AGNode) {
    this.users0 += other
    //other.uses0 += this
  }

  def users_-=(user : AGNode){
    users0.remove(user)
    //user.uses0.remove(this)
  }

  def users: mutable.Iterable[AGNode] = users0

  def isUsedBy(other : AGNode) = users0.contains(other)

  /* a primary use is for example the use of a class when declaring a variable or a parameter
   a side use is in this example a call to a method with the same variable/parameter
      the use of the method is a side use of the declaration
       a couple primary use/ side use is a use dependency
 */

  /*(this, key) is a primary uses and sidesUses(key) are the corresponding side uses */
  private [this] val sideUses0 : mutable.Map[AGNode, mutable.Set[AGEdge]] = mutable.Map()

  def hasSideUses = !sideUses0.isEmpty
  def usesMapString( map : mutable.Map[AGNode, mutable.Set[AGEdge]],
                     keyType : String,
                     contentType : String) : String = {
    map.map{
      case (key, content) =>
        val contentStr =
          if(content.isEmpty){"(no "+ contentType+")\n"}
          else {
            content.mkString("\n" + contentType + " :\n\t", "\n\t", "\n")
          }

        keyType + " : (" + this + ", " + key + ")" + contentStr

    }.mkString("")
  }
  def sideUsesString = usesMapString(sideUses0, "primary", "secondaries")
  def primaryUsesString = usesMapString(primaryUses0, "secondary", "primary")

  def sideUses(primaryUsee : AGNode) : Option[Iterable[AGEdge]] = sideUses0.get(primaryUsee)

  def sideUses_+=(primaryUsee : AGNode, sideUse : AGEdge) = {
    sideUses_++=(primaryUsee, mutable.Set(sideUse))
  }

  def sideUses_++=(primaryUsee : AGNode, sideUses: mutable.Set[AGEdge]){
    sideUses0 get primaryUsee match {
      case None =>
        sideUses0 += (primaryUsee -> sideUses)
      case Some(s) =>
        sideUses0 += (primaryUsee -> s.++=(sideUses))
    }
    sideUses.foreach{ _.create() }
    primaryUsee.users_+=(this)
  }

  def sideUses_-=(primaryUsee : AGNode){
    sideUses0 get primaryUsee match {
      case None => ()
      case Some(sideUses) =>
        sideUses.foreach{ _.delete() }
        sideUses0.remove(primaryUsee)
    }

  }

  def sideUses_-=(primaryUsee : AGNode, sideUse : AGEdge){
    val side_uses = sideUses0(primaryUsee)
    side_uses.remove(sideUse)
    sideUse.delete()
    if(side_uses.isEmpty)
      sideUses0.remove(primaryUsee)
    primaryUsee.users_-=(this)
  }


  /*(this, key) is a side uses and primaryUses(key) is the corresponding primary uses */
  private [this] val primaryUses0 : mutable.Map[AGNode, mutable.Set[AGEdge]] = mutable.Map()

  def hasPrimaryUses = !primaryUses0.isEmpty
  def primaryUses(sideUsee : AGNode) : Option[Iterable[AGEdge]] =
    primaryUses0.get(sideUsee)

  def primaryUses_+=(sideUsee : AGNode, primaryUse : AGEdge) = {
    primaryUses0 get sideUsee match {
      case None =>
        primaryUses0 += (sideUsee -> mutable.Set(primaryUse))
      case Some(s) =>
        primaryUses0 += (sideUsee -> s.+=(primaryUse))
    }
    primaryUse.create()
    sideUsee.users_+=(this)
  }

  def primaryUses_-=(sideUsee: AGNode){
    primaryUses0 get sideUsee match {
      case None => ()
      case Some( primUses ) =>
        primUses.foreach(_.delete())
        primaryUses0.remove(sideUsee)

    }
    sideUsee.users_-=(this)
  }


  def findNewPrimaryUsee(primaryUsee : AGNode,
                         newSideUsee : AGNode,
                         policy : RedirectionPolicy) : AGNode = {
    policy match {
      case Move() => newSideUsee.container_!
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
        if(! primary_uses.tail.isEmpty) {
          println("redirecting side uses : (" + this + ", " + sideUsee
            + ") to (" + this + ", " + newUsee + ")")
          println("primary uses are ")
          println( primary_use)
          primary_uses.tail.mkString("\n")
          throw new AGError("Do not know how to unsuscribe a side use with multiple primary uses")
        }
        else {

          val primUser = primary_use.source
          primUser.sideUses_-=(primary_use.target, AGEdge.uses(this, sideUsee))
          primaryUses_-=(sideUsee)

          val newPrimUsee = findNewPrimaryUsee(primary_use.target,
            newUsee, policy)

          primUser.sideUses_+=(newPrimUsee, AGEdge.uses(this, newUsee))
          primaryUses_+=(newUsee, AGEdge.uses(primUser, newPrimUsee))
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

        sideUses_-=(primaryUsee)

        sideUses_++=(newUsee, new_side_uses)
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
        this.container = Some(n)
        n
    }
  }


  def moveTo(newContainer : AGNode) {
    detach()
    this.container = Some(newContainer)
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
  private var friendsSet : mutable.Set[AGNode] = mutable.Set()

  def friends : mutable.Iterable[AGNode] = friendsSet
  def friends_++= = friendsSet ++= _

  /**
   * this scope is hidden
   */
  private var scopeConstraints: mutable.Buffer[ScopeConstraint]= mutable.Buffer()

  def scopeConstraints_+=(ct : ScopeConstraint) = scopeConstraints += ct

  def scopeConstraints_+=(facades : List[AGNode],
                          interlopers : List[AGNode],
                          friends : List[AGNode]) =
    scopeConstraints += new ScopeConstraint(this, facades, interlopers, friends)


  /**
   * this element is hidden but not the elements that it contains
   */
  private var elementConstraints : mutable.Buffer[ElementConstraint]= mutable.Buffer()

  def elementConstraints_+=(x : ElementConstraint) = elementConstraints += x

  def elementConstraints_+=(interlopers : List[AGNode],
                            friends : List[AGNode]) =
    elementConstraints += new ElementConstraint(this, interlopers, friends)

  /**
   * Constraints Handling
   */

  def discardConstraints() {
    friendsSet = mutable.Set()
    scopeConstraints = mutable.Buffer()
    elementConstraints = mutable.Buffer()
  }

  def constraintsString : String = {

    def ifNotEmpty(coll: mutable.Iterable[_ <: Any], str: => String) : String =
      if (coll.isEmpty) ""
      else str

    ifNotEmpty(friendsSet,
      "areFriendsOf([" + friendsSet.mkString(", ") + "], " + this + ").\n") +
      ifNotEmpty(scopeConstraints, scopeConstraints.mkString("", "\n", "\n")) +
      ifNotEmpty(elementConstraints, elementConstraints.mkString("", "\n", "\n"))
  }


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

  private def violated(originalTarget : AGNode)(ct : ScopeConstraint) : Boolean =
    ct.interlopers.hasScopeThatContains_*(this) &&
      !(ct.friends.hasScopeThatContains_*(this) ||
        ct.facades.hasScopeThatContains_*(originalTarget))

  def violatesScopeConstraintsOf(other0 : AGNode) : List[ScopeConstraint] = {
    def aux(other : AGNode, acc : List[ScopeConstraint]) : List[ScopeConstraint] = {
      val acc2 = if(!other.contains_*(this))
        other.scopeConstraints.filter(violated(other0)).toList ::: acc
      else acc

      other.container match {
        case None => acc2
        case Some(p) => aux(p, acc2)
      }
    }
    aux(other0,List())
  }

  def potentialScopeInterloperOf(other0 : AGNode) : Boolean = {
    @tailrec
    def aux(other: AGNode): Boolean =
      !other.contains_*(this) &&
        other.scopeConstraints.exists(violated(other0)) ||
        (other.container match {
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

  private def violated(ct : ElementConstraint) : Boolean =
    ct.interlopers.hasScopeThatContains_*(this) &&
      !ct.friends.hasScopeThatContains_*(this)

  def violatesElementConstraintOf(other : AGNode) =
    other.elementConstraints.filter(violated).toList

  def potentialElementInterloperOf(other:AGNode) =
    other.elementConstraints.exists(violated)

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

  protected var abstractions0: mutable.Set[(AGNode, AbstractionPolicy)]= mutable.Set()
  def abstractions : mutable.Iterable[(AGNode, AbstractionPolicy)] = abstractions0

  def searchExistingAbstractions(){}


  def createNodeAbstraction(abskind :  NodeKind, policy : AbstractionPolicy) : AGNode = {
    // TODO find a strategy or way to make the user choose which abstractkind is used !
    val n = graph.addNode(name + "_abstraction", abskind)
    abstractions0 += ((n, policy))
    /* little hack (?) : an abstraction is its own abstraction otherwise,
     on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ...
     and that can go on... */
    n.abstractions0 += ((n, policy))
    n
  }

  def createAbstraction(abskind : NodeKind , policy : AbstractionPolicy) : AGNode = {
    val abs = createNodeAbstraction(abskind, policy)
    users_+=(abs)
    abs
  }

  def addHideFromRootException(friend : AGNode){
    def addExc(sct : Constraint) {
      if (sct.interlopers.head == graph.root)
        sct friends_+= friend
    }

    scopeConstraints foreach addExc
    elementConstraints foreach addExc
  }

}



