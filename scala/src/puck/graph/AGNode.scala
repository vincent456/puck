package puck.graph

import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.collection.mutable
import puck.graph.constraints._


class AGNodeIterator (val root : AGNode)extends BreadthFirstTreeIterator[AGNode]


trait AGNodeBuilder {
  def apply(g: AccessGraph, id: Int, name:String, kind : NodeKind) : AGNode
  def makeKey(fullName: String, localName:String, kind: NodeKind) : String
  def kinds : List[NodeKind]
  val scopeSeparator : String = "."
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
              val kind: NodeKind) extends HasChildren[AGNode]{
  //extends Iterable[AGNode]{

  /*override def equals(obj: Any): Boolean = obj match {
    case that: AGNode =>
      if (this.graph eq that.graph)
       this.id == that.id
      else {

        def softEqual(n1 : AGNode, n2 :AGNode) =
          n1.name == n2.name && n1.kind == n2.kind //TODO see type equality ...
        //TODO use soft equality to compare uses arcs

        softEqual(this, that) &&
        this.name == that.name && this.kind == that.kind &&
          content.forall { c =>
            that.content.find(tc => tc == c)
          }
      }
    case _ => false
  }

  override def hashCode: Int = (this.id + kind.hashCode()) / 41*/

  def softEqual(other : AGNode) : Boolean = {
    //this.name == other.name && // NO !!
    this.kind == other.kind &&
    users.forall{n => other.users.exists(_.softEqual(n))}  &&
    content.forall{n => other.content.exists(_.softEqual(n))}
  }

  /**
   * relies on the contains tree : do not modify it while traversing
   */
  def iterator = new AGNodeIterator(this)

  override def toString: String = "(" + fullName + ", " + kind + ")"

  def nameTypeString = name + (kind match{case k : HasType[_] => " : " + k.`type`; case _ => ""})


  def fullName: String = {

    /*if (isRoot) nameTypeString
      else {*/
    val path = containerPath.map(_.nameTypeString)
    (if (path.head == AccessGraph.rootName)
      path.tail
    else
      AccessGraph.unrootedStringId :: path ).mkString(graph.scopeSeparator)
  }

  var isMutable = true

  /**
   * Arcs
   */

  private var container0 : AGNode = this

  def container = container0

  def isRoot = container == this

  def detach(){
    if(container0 != this)
      container.content_-=(this)
    /*container0.content0 -= this
    container0 = this*/
  }

  def canContain(n : AGNode) : Boolean = {
    n != this &&
      (this.kind canContain n.kind) &&
      this.isMutable
  }

  private val content0 : mutable.Set[AGNode] = mutable.Set()

  def content : mutable.Iterable[AGNode] = content0

  def children = content

  def content_+=(n:AGNode) {
    n.container0 = this
    this.content0.add(n)
    graph.transformations.addEdge(AGEdge.contains(this, n))
  }
  def content_-=(n:AGNode) {
    n.container0 = n
    this.content0.remove(n)
    graph.transformations.removeEdge(AGEdge.contains(this, n))
  }

  /*def content_+=(n:AGNode) {
    if( n.container0 == this)
      throw new IllegalAGOperation("content += error " + n + "container is already "+ this)

    n.container0 = this

    if(!this.content0.add(n))
      throw new IllegalAGOperation("content += error "+ this +" already contains "+ n)

    graph.transformations.addEdge(AGEdge.contains(this, n))

  }

  def content_-=(n:AGNode) {

    if( n.container0 != this)
      throw new IllegalAGOperation("content -= error " +n + "container is not "+ this)

    n.container0 = n

    if(!this.content0.remove(n))
      throw new IllegalAGOperation("content -= error "+ this +" does not contains "+ n)


    graph.transformations.removeEdge(AGEdge.contains(this, n))
  }*/

  def moveTo(newContainer : AGNode) {
    container content_-= this
    newContainer content_+= this

    users.foreach{
      _.redirectPrimaryUses(this, this, Move())
    }
  }

  private[graph] def isContentEmpty = content0.isEmpty

  def contains(other :AGNode) = content0.contains(other)

  def contains_*(other:AGNode) : Boolean =
    other == this || !other.isRoot && this.contains_*(other.container)


  def containerPath(ancestor : AGNode) : List[AGNode] = {

    def aux(current : AGNode, acc : List[AGNode]) : List[AGNode] =
      if (ancestor == current) this :: acc
      else if (current.isRoot) throw new AGError("container path computation error :"+
        "\nfollowed path : " + acc.foldRight(List[String]()){
        case (n, acc0) => n.name :: acc0} +
        "\ncurrent node : " + current.name +
        "\nancestor " + ancestor.name + " not found !")
      else aux(current.container, current :: acc)

    aux(this, List())
  }

  def containerPath : List[AGNode] = {

    def aux(current : AGNode, acc : List[AGNode]) : List[AGNode] =
      if (current.isRoot) current :: acc
      else aux(current.container, current :: acc)

    aux(this, List())
  }

  private [this] val superTypes0 : mutable.Set[AGNode] = mutable.Set()

  def isSuperTypeOf(other : AGNode) : Boolean = {
    other.superTypes.exists(_ == this) ||
      other.superTypes.exists(_.isSuperTypeOf(this))
  }


  def superTypes : Iterable[AGNode] = superTypes0
  private val subTypes0 : mutable.Set[AGNode] = mutable.Set()
  def subTypes : Iterable[AGNode] = subTypes0

  def superTypes_+=(st:AGNode) {
    if(superTypes0.add(st)) {
      st.subTypes0.add(this)
      graph.transformations.addEdge(AGEdge.isa(this, st))
      abstractions_+=(st, SupertypeAbstraction())
    }
  }

  def superTypes_-=(st:AGNode) {

    if(superTypes0.remove(st)) {
      st.subTypes0.remove(this)
      graph.transformations.removeEdge(AGEdge.isa(this, st))
      abstractions_-=(st, SupertypeAbstraction())
    }
  }

  def isa(other : AGNode) : Boolean = {
    superTypes.exists( n => n == other)
  }

  //TODO think about removing uses0 & users0 and keep only the sideUses0/primaryUses0 maps
  //private var uses0 : mutable.Set[AGNode] = mutable.Set()
  def uses(other : AGNode) = other.isUsedBy(this)// uses0.contains(other)
  def uses_+=(other: AGNode) = other.users_+=(this)
  def uses_-=(other: AGNode) = other.users_-=(this)

  private [this] val users0 : mutable.Set[AGNode] = mutable.Set()
  def users_+=(other : AGNode) {
    if(users0.add(other))
      graph.transformations.addEdge(AGEdge.uses(other, this))
  }
  def users_-=(user : AGNode){
    if(users0.remove(user))
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

  def findNewPrimaryUsee(currentPrimaryUsee : AGNode,
                         newSideUsee : AGNode,
                         policy : RedirectionPolicy) : AGNode = {
    policy match {
      case Move() => newSideUsee.container
      case absPolicy : AbstractionPolicy =>
        currentPrimaryUsee.abstractions.find{
          case (node, `absPolicy`) => node.contains_*(newSideUsee)
          case _ => false
        } match {
          case Some((n, _)) => n
          case None =>
            graph.iterator.find{ node =>
              node.contains_*(newSideUsee) &&
                currentPrimaryUsee.kind.abstractKinds(absPolicy).contains(node.kind)
            } match {
              case Some(n) => n
              case None => throw new AGError("no correct primary abstraction found !")
            }
        }
    }
  }

  def redirectPrimaryUses(currentSideUsee : AGNode,
                          newSideUsee : AGNode,
                          policy : RedirectionPolicy){
    println("redirecting primary uses ... ")
    primaryUses get currentSideUsee match {
      case None =>
        println("no primary uses to redirect")
        ()
      case Some(primary_uses) =>
        println("uses to redirect:%s".format(primary_uses.mkString("\n", "\n","\nend of list")))

        assert(primary_uses.nonEmpty)

        val primary = primary_uses.head
        if(primary_uses.tail.nonEmpty) {
          println("redirecting side uses : (%s, %s) to (%s, %s)".format(this, currentSideUsee, this, newSideUsee))
          println("primary uses are ")
          primary_uses.mkString("-", "\n-", "\n")
          throw new AGError("Do not know how to unsuscribe a side use with multiple primary uses")
        }
        else {
          val newPrimary = AGEdge.uses(primary.user,
            findNewPrimaryUsee(primary.usee, newSideUsee, policy))


          graph.removeUsesDependency(primary, AGEdge.uses(this, currentSideUsee))

          //TODO redirection en cascade !!!
          primary.delete()
          newPrimary.create()

          graph.addUsesDependency(newPrimary, AGEdge.uses(this, newSideUsee))

        }
    }
  }

  def redirectSideUses(currentPrimaryUsee: AGNode,
                       newPrimaryUsee : AGNode,
                       policy : RedirectionPolicy){
    println("redirecting side uses ... ")
    sideUses get currentPrimaryUsee match {
      case None =>
        println("no side uses to redirect")
        ()
      case Some( sides_uses ) =>
        println("uses to redirect:%s".format(sides_uses.mkString("\n", "\n","\nend of list")))

        sides_uses foreach {
          side =>
            side.usee.abstractions.find {
              case (abs, _) => newPrimaryUsee.contains(abs)
              case _ => false
            } match {
              case None =>
                throw new RedirectionError(("While redirecting primary uses (%s, %s) to (%s, %s)\n" +
                "no satisfying abstraction to redirect side use %s").
                format( this, currentPrimaryUsee, this, newPrimaryUsee, side))

              case Some( (new_side_usee, _) ) =>
                val newSide = AGEdge.uses(side.user, new_side_usee)

                graph.removeUsesDependency(AGEdge.uses(this, currentPrimaryUsee), side)
                //TODO redirection en cascade !!!
                side.delete()
                newSide.create()
                graph.addUsesDependency(AGEdge.uses(this, newPrimaryUsee), newSide)

            }
        }
    }
  }

  def redirectUses(oldUsee : AGNode, newUsee : AGNode,
                   policy : RedirectionPolicy){

    println("redirecting uses %s --> %s to\n%s (%s)".format(this, oldUsee, newUsee, policy))
    oldUsee users_-= this
    newUsee users_+= this

    redirectPrimaryUses(oldUsee, newUsee, policy)
    redirectSideUses(oldUsee, newUsee, policy)
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

  /*private[this] lazy val abstractions0: mutable.Set[(AGNode, AbstractionPolicy)] =
    searchExistingAbstractions()
  def searchExistingAbstractions() = mutable.Set[(AGNode, AbstractionPolicy)]()*/

  private[this] val abstractions0 = mutable.Set[(AGNode, AbstractionPolicy)]()

  def abstractions : mutable.Iterable[(AGNode, AbstractionPolicy)] = abstractions0
  def abstractions_-=(n : AGNode, p : AbstractionPolicy){
    if(abstractions0.remove( (n,p) ))
      graph.transformations.unregisterAbstraction(this, n, p)

  }
  def abstractions_+= (n : AGNode, p : AbstractionPolicy){
    if(abstractions0.add( (n,p) ))
      graph.transformations.registerAbstraction(this, n, p)
  }

  def abstractionName(abskind :  NodeKind, policy : AbstractionPolicy) : String =
    name + "_" + policy

  def createNodeAbstraction(abskind :  NodeKind, policy : AbstractionPolicy) : AGNode = {
    val n = graph.addNode(abstractionName(abskind, policy), abskind)
    abstractions_+=(n, policy)
    n
  }

  def createAbstraction(abskind : NodeKind , policy : AbstractionPolicy) : AGNode = {
    val abs = createNodeAbstraction(abskind, policy)
    policy match {
      case SupertypeAbstraction() =>  abs users_+= this
      case DelegationAbstraction() => this users_+= abs
    }
    abs
  }

  def addHideFromRootException(friend : AGNode){
    def addExc(ct : ConstraintWithInterlopers) {
      if (ct.interlopers.iterator.contains(graph.root)){
        ct.friends += friend
        graph.transformations.addFriend(ct, friend)
      }
    }

    scopeConstraints foreach addExc
    elementConstraints foreach addExc
  }

}



