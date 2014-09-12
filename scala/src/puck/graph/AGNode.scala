package puck.graph

import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.language.implicitConversions
import scala.collection.mutable
import puck.graph.constraints._


class AGNodeIterator[Kind <: NodeKind[Kind]] (val root : AGNode[Kind])extends BreadthFirstTreeIterator[AGNode[Kind]]


trait AGNodeBuilder[Kind <: NodeKind[Kind]] {
  def apply(g: AccessGraph[Kind], id: Int, name:String, kind : Kind) : AGNode[Kind]
  def makeKey(fullName: String, localName:String, kind: Kind) : String
  def rootKind : Kind
  def kinds : List[Kind]
  val scopeSeparator : String = "."
}

object AGNode extends AGNodeBuilder[VanillaKind]{
  override def apply(g: AccessGraph[VanillaKind],
                     id: Int, name : String,
                     kind : VanillaKind) : AGNode[VanillaKind] = new AGNode(g, id, name, kind)

  def rootKind = VanillaRoot()

  override def makeKey(fullName: String, localName:String,
                       kind : VanillaKind) : String = fullName

  val kinds = List(VanillaNodeKind())

  def implUsesAbs(implKind : VanillaKind, absKind : VanillaKind) : Boolean =
    (implKind, absKind) match {
      case (VanillaNodeKind(), VanillaNodeKind()) => false
      case _ => throw new AGError("do not know if impl (%s) uses abs (%s)".format(implKind, absKind))
    }


}

class AGNode[Kind <: NodeKind[Kind]] (val graph: AccessGraph[Kind],
                                      val id: Int,
                                      var name: String,
                                      val kind: Kind) extends HasChildren[AGNode[Kind]]{

  type NodeType = AGNode[Kind]
  //extends Iterable[NodeType]{
  kind.node = this

  /*override def hashCode: Int = (this.id + kind.hashCode()) / 41*/

  /**
   * relies on the contains tree : do not modify it while traversing
   */
  def iterator = new AGNodeIterator(this)

  def distance(other : AGNode[Kind]) = {
    if(this == other) 0
    else if(this contains_*  other)
      other.containerPath(this).length - 1
    else if (other contains_* this)
      this.containerPath(other).length  - 1
    else {
      val thisPathToRoot = this.containerPath.reverse
      val otherPathToRoot = other.containerPath.reverse
      println(thisPathToRoot)
      println(otherPathToRoot)
      thisPathToRoot.foldLeft[Option[AGNode[Kind]]](None) {
        case (sn@Some(_), _) => sn
        case (None, n) =>
          if (otherPathToRoot contains n)
            Some(n)
          else
            None

      } match {
        case None =>
          Int.MaxValue
        case Some(commonAncestor) =>
          val count: ((Int, Boolean), AGNode[Kind]) => (Int, Boolean) = {
            case ((i, alreadyFound), n) =>
              if (alreadyFound || n == commonAncestor) (i, true)
              else (i + 1, false)
          }

          (thisPathToRoot.foldLeft((0, false))(count),
            otherPathToRoot.foldLeft((0, false))(count)) match {
            case ((i, _), (j, _)) => i + j
          }
      }
    }

  }

  override def toString: String = kind + "(" + fullName + ")"

  def nameTypeString = name + (kind match{case k : HasType[_, _] => " : " + k.`type`; case _ => ""})

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

  private var container0 : NodeType = this

  def container = container0

  //def isRoot = graph.root == this

  def isRoot = container == this

  def canContain(n : NodeType) : Boolean = {
    n != this &&
      (this.kind canContain n.kind) &&
      this.isMutable
  }


  object content {

    private val content0 : mutable.Set[NodeType] = mutable.Set()

    def apply() : mutable.Iterable[NodeType] = content0

    implicit def iterableContent(c : AGNode.this.content.type ) : mutable.Iterable[NodeType] = content0

    def contains(n : NodeType) = content0.contains(n)

    def +=(n : NodeType, register : Boolean = true){
      n.container0 = AGNode.this
      content0.add(n)
      if(register)
        graph.transformations.addEdge(AGEdge.contains(AGNode.this, n))
    }

    def -=(n : NodeType, register : Boolean = true){
      n.container0 = n
      content0.remove(n)
      if(register)
        graph.transformations.removeEdge(AGEdge.contains(AGNode.this, n))
    }
  }

  def children = content()

  //java accessor
  def contentAdd(n : NodeType, register : Boolean) = content.+=(n, register)



  def moveTo(newContainer : NodeType) {
    AGEdge.contains(container, this).changeSource(newContainer)

    users.foreach{
      _.redirectPrimaryUses(this, this, Move())
    }
  }

  def contains(other :NodeType) = content.contains(other)

  def contains_*(other:NodeType) : Boolean =
    other == this || !other.isRoot && this.contains_*(other.container)


  def containerPath(ancestor : NodeType) : List[NodeType] = {

    def aux(current : NodeType, acc : List[NodeType]) : List[NodeType] =
      if (ancestor == current) this :: acc
      else if (current.isRoot) throw new AGError("container path computation error :"+
        "\nfollowed path : " + acc.foldRight(List[String]()){
        case (n, acc0) => n.name :: acc0} +
        "\ncurrent node : " + current.name +
        "\nancestor " + ancestor.name + " not found !")
      else aux(current.container, current :: acc)

    aux(this, List())
  }

  def containerPath : List[NodeType] = {

    def aux(current : NodeType, acc : List[NodeType]) : List[NodeType] =
      if (current.isRoot) current :: acc
      else aux(current.container, current :: acc)

    aux(this, List())
  }

  private [this] val superTypes0 : mutable.Set[NodeType] = mutable.Set()

  def isSuperTypeOf(other : NodeType) : Boolean = {
    other.superTypes.exists(_ == this) ||
      other.superTypes.exists(_.isSuperTypeOf(this))
  }


  def superTypes : Iterable[NodeType] = superTypes0
  private val subTypes0 : mutable.Set[NodeType] = mutable.Set()
  def subTypes : Iterable[NodeType] = subTypes0

  def superTypes_+=(st:NodeType, register : Boolean = true) {
    if(superTypes0.add(st)) {
      st.subTypes0.add(this)
      if(register)
        graph.transformations.addEdge(AGEdge.isa(this, st))
      abstractions_+=(st, SupertypeAbstraction())
    }
  }

  def superTypes_-=(st:NodeType, register : Boolean = true) {

    if(superTypes0.remove(st)) {
      st.subTypes0.remove(this)
      if(register)
        graph.transformations.removeEdge(AGEdge.isa(this, st))
      abstractions_-=(st, SupertypeAbstraction())
    }
  }

  def isa(other : NodeType) : Boolean = {
    superTypes.exists( n => n == other)
  }

  val uses = usesObject

  object usesObject {
    private [AGNode] val uses0 = mutable.Set[NodeType]()
    def apply() : mutable.Iterable[NodeType] = uses0
    implicit def iterableUses(c : AGNode.this.uses.type ) : mutable.Iterable[NodeType] = uses0
    def apply(other : NodeType) = uses0.contains(other)
    def += (usee: NodeType, register : Boolean = true) = usee.users += (AGNode.this, register)
    def -= (usee: NodeType, register : Boolean = true) = usee.users -= (AGNode.this, register)
  }

  val users = usersObject

  object usersObject {
    private val users0 = mutable.Set[NodeType]()
    def apply() : mutable.Iterable[NodeType] = users0

    implicit def iterableUsers(c : AGNode.this.users.type ) : mutable.Iterable[NodeType] = users0
    def contains(n : NodeType) = users0.contains(n)

    def +=(user : NodeType, register : Boolean = true) {
      if(users0.add(user) && register) {
        graph.transformations.addEdge(AGEdge.uses(user, AGNode.this))
      }
      user.uses.uses0.add(AGNode.this)
    }

    def -=(user : NodeType, register : Boolean = true) {
      if(users0.remove(user) && register) {
        graph.transformations.removeEdge(AGEdge.uses(user, AGNode.this))
      }
      user.uses.uses0.remove(AGNode.this)

    }
  }

  //java accessor
  def userAdd(n : NodeType, register : Boolean) = users.+=(n, register)

  /* a primary use is for example the use of a class when declaring a variable or a parameter
   a side use is in this example a call to a method with the same variable/parameter
      the use of the method is a side use of the declaration
       a couple primary use/ side use is a use dependency
 */

  /*(this, key) is a primary uses and sidesUses(key) are the corresponding side uses */
  val sideUses = new UsesDependencyMap(this, Dominant())

  /*(this, key) is a side uses and primaryUses(key) is the corresponding primary uses */
  val primaryUses = new UsesDependencyMap(this, Dominated())

  def findNewPrimaryUsee(currentPrimaryUsee : NodeType,
                         newSideUsee : NodeType,
                         policy : RedirectionPolicy) : NodeType = {
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

  def redirectPrimaryUses(currentSideUsee : NodeType,
                          newSideUsee : NodeType,
                          policy : RedirectionPolicy){
    graph.logger.writeln("redirecting primary uses ... ",3)
    primaryUses get currentSideUsee match {
      case None =>
        graph.logger.writeln("no primary uses to redirect",3)
        ()
      case Some(primary_uses) =>
        graph.logger.writeln("uses to redirect:%s".format(primary_uses.mkString("\n", "\n","\nend of list")),3)

        assert(primary_uses.nonEmpty)

        val primary = primary_uses.head
        if(primary_uses.tail.nonEmpty) {
          println("redirecting side uses (%s, %s) target to %s".format(this, currentSideUsee, newSideUsee))
          println("primary uses are ")
          primary_uses.mkString("-", "\n-", "\n")
          throw new AGError("Do not know how to unsuscribe a side use with multiple primary uses")
        }
        else {

          graph.removeUsesDependency(primary, AGEdge.uses(this, currentSideUsee))

          val newPrimary = primary.user.redirectUses(primary.usee,
            findNewPrimaryUsee(primary.usee, newSideUsee, policy),  policy)

          graph.addUsesDependency(newPrimary, AGEdge.uses(this, newSideUsee))

        }
    }
  }

  def redirectSideUses(currentPrimaryUsee: NodeType,
                       newPrimaryUsee : NodeType,
                       policy : RedirectionPolicy){
    graph.logger.writeln("redirecting side uses ... ", 3)
    sideUses get currentPrimaryUsee match {
      case None =>
        graph.logger.writeln("no side uses to redirect", 3)
        ()
      case Some( sides_uses ) =>
        graph.logger.writeln("uses to redirect:%s".format(sides_uses.mkString("\n", "\n","\nend of list")),3)

        sides_uses foreach {
          side =>
            side.usee.abstractions.find {
              case (abs, _) => newPrimaryUsee.contains(abs)
              case _ => false
            } match {
              case None =>
                throw new RedirectionError(("While redirecting primary uses (%s, %s) target to %s\n" +
                  "no satisfying abstraction to redirect side use %s").
                  format( this, currentPrimaryUsee, newPrimaryUsee, side))

              case Some( (new_side_usee, _) ) =>

                graph.removeUsesDependency(AGEdge.uses(this, currentPrimaryUsee), side)

                val newSide = side.user.redirectUses(side.usee, new_side_usee, policy)

                graph.addUsesDependency(AGEdge.uses(this, newPrimaryUsee), newSide)

            }
        }
    }
  }

  def redirectUses(oldUsee : NodeType, newUsee : NodeType,
                   policy : RedirectionPolicy) = {
    if(oldUsee == newUsee)
      AGEdge.uses(this, oldUsee)

    else if(this uses oldUsee) {

      graph.logger.writeln("redirecting %s target to\n%s (%s)".format(AGEdge.uses(this, oldUsee), newUsee, policy), 3)
      val newUses = AGEdge.uses(this, oldUsee).changeTarget(newUsee)

      this.kind match {
        case k : HasType[Kind, _] => k.redirectUses(oldUsee, newUsee)
        case _ => ()
      }

      redirectPrimaryUses(oldUsee, newUsee, policy)
      redirectSideUses(oldUsee, newUsee, policy)

      newUses
    }
    else if (this uses newUsee) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      graph.logger.writeln("redirecting uses %s --> %s to\n%s (%s) : FAILURE !! %s is not used".format(this, oldUsee, newUsee, policy, oldUsee))
      AGEdge.uses(this, newUsee)
    }
    else {
      if(oldUsee.users.contains(this) || newUsee.users.contains(this))
        throw new AGError("incoherent state !!!!!!!!!!!!")

      throw new AGError(("redirecting uses %s --> %s to\n%s (%s)\n" +
        "!!! nor the oldUsee or the newUsee is really used !!! ").format(this, oldUsee, newUsee, policy))
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
  val friendConstraints = new ConstraintSet[Kind, FriendConstraint[Kind]]()
  /*
   * assert owners contains this
   */
  /**
   * this scope is hidden
   */
  val scopeConstraints = new ConstraintSet[Kind, ScopeConstraint[Kind]]()

  /**
   * this element is hidden but not the elements that it contains
   */
  val elementConstraints = new ConstraintSet[Kind, ElementConstraint[Kind]]()

  /**
   * Constraints Handling
   */

  def discardConstraints() {
    friendConstraints.clear()
    scopeConstraints.clear()
    elementConstraints.clear()
  }

  def remove(ct : Constraint[Kind]) = ct match {
    case fct @ FriendConstraint(_,_) => friendConstraints -= fct
    case ect @ ElementConstraint(_,_,_) => elementConstraints -= ect
    case sct @ ScopeConstraint(_,_,_,_) => scopeConstraints -= sct
  }

  final def friendOf(other : NodeType) : Boolean = other.friendConstraints.hasFriendScopeThatContains_*( this ) ||
    !other.isRoot && friendOf(other.container)

  def violatedScopeConstraintsOf(usee0 : NodeType) : List[ScopeConstraint[Kind]] = {
    val uses = AGEdge.uses(this, usee0)

    def aux(usee : NodeType, acc : List[ScopeConstraint[Kind]]) : List[ScopeConstraint[Kind]] = {
      val acc2 = if(!usee.contains_*(this))
        usee.scopeConstraints.filter(_.violated(uses)).toList ::: acc
      else acc

      if(usee.isRoot) acc2
      else aux(usee.container, acc2)
    }
    aux(usee0,List())
  }


  def potentialScopeInterloperOf(usee0 : NodeType) : Boolean = {
    val uses = AGEdge.uses(this, usee0)

    def aux(usee: NodeType): Boolean =
      !usee.contains_*(this) &&
        usee.scopeConstraints.exists(_.violated(uses)) ||
        !usee.isRoot && aux(usee.container)

    aux(usee0)
  }

  def violatedElementConstraintOf(usee : NodeType) =
    usee.elementConstraints.filter(_.violated(AGEdge.uses(this, usee))).toList

  def potentialElementInterloperOf(usee:NodeType) =
    usee.elementConstraints.exists(_.violated(AGEdge.uses(this, usee)))

  def interloperOf(other : NodeType) =
    (potentialScopeInterloperOf(other)
      || potentialElementInterloperOf(other)) && !friendOf(other)

  def isWronglyContained : Boolean = !isRoot && (container interloperOf this)

  def wrongUsers : List[NodeType] = {
    users.foldLeft(List[NodeType]()){(acc:List[NodeType], user:NodeType) =>
      if( user interloperOf this ) user :: acc
      else acc
    }
  }

  /**
   * Solving
   */

  /*private[this] lazy val abstractions0: mutable.Set[(NodeType, AbstractionPolicy)] =
    searchExistingAbstractions()
  def searchExistingAbstractions() = mutable.Set[(NodeType, AbstractionPolicy)]()*/

  private[this] val abstractions0 = mutable.Set[(NodeType, AbstractionPolicy)]()

  def abstractions : mutable.Iterable[(NodeType, AbstractionPolicy)] = abstractions0
  def abstractions_-=(n : NodeType, p : AbstractionPolicy){
    if(abstractions0.remove( (n,p) ))
      graph.transformations.unregisterAbstraction(this, n, p)

  }
  def abstractions_+= (n : NodeType, p : AbstractionPolicy){
    if(abstractions0.add( (n,p) ))
      graph.transformations.registerAbstraction(this, n, p)
  }

  def abstractionName(abskind : Kind, policy : AbstractionPolicy) : String =
    name + "_" + policy

  def createNodeAbstraction(abskind :  Kind, policy : AbstractionPolicy) : NodeType = {
    val n = graph.addNode(abstractionName(abskind, policy), abskind)
    abstractions_+=(n, policy)
    n
  }

  def createAbstraction(abskind : Kind , policy : AbstractionPolicy) : NodeType = {
    val abs = createNodeAbstraction(abskind, policy)
    policy match {
      case SupertypeAbstraction() =>  abs.users += this
      case DelegationAbstraction() => this.users += abs
    }
    abs
  }

  def addHideFromRootException(friend : NodeType){
    def addExc(ct : ConstraintWithInterlopers[Kind]) {
      if (ct.interlopers.iterator.contains(graph.root)){
        ct.friends += friend
        graph.transformations.addFriend(ct, friend)
      }
    }

    scopeConstraints foreach addExc
    elementConstraints foreach addExc
  }


  def searchMergingCandidate() : Option[NodeType] = None

  def mergeWith(other : NodeType){

    other.users.foreach { user =>
      AGEdge.uses(user, other).changeTarget(this)
      user.kind match {
        case k : HasType[Kind, _] => k.redirectUses(other, this)
        case _ => ()
      }
    }

    other.uses.foreach { AGEdge.uses(other, _).changeSource(this) }

    other.superTypes.foreach { AGEdge.isa(other, _).changeSource(this) }

    other.subTypes.foreach { AGEdge.isa(_, other).changeTarget(this) }

    other.primaryUses.foreach{
      case (sideUsee, primUses) =>
        primUses.foreach { pUse =>
          graph.addUsesDependency(pUse, AGEdge.uses(this, sideUsee))
          graph.removeUsesDependency(pUse, AGEdge.uses(other, sideUsee))
        }
    }
    other.sideUses.foreach{
      case (primeUsee, sidUses) =>
        sidUses.foreach { sUse =>
          graph.addUsesDependency(AGEdge.uses(this, primeUsee), sUse)
          graph.removeUsesDependency(AGEdge.uses(other, primeUsee), sUse)
        }
    }
    AGEdge.contains(other.container, other).delete()
    graph.remove(other)
  }

  private def outgoingDependencies(root : AGNode[Kind], acc0 : Set[AGEdge[Kind]]) : Set[AGEdge[Kind]]= {
    val acc1 = uses.foldLeft(acc0){
      (acc, usee) =>
        if(root contains_* usee) acc
        else acc + AGEdge.uses(this, usee)
    }
    content.foldLeft(acc1){(acc, child) => child.outgoingDependencies(root, acc)}
  }

  def outgoingDependencies : Set[AGEdge[Kind]] = outgoingDependencies(this, Set[AGEdge[Kind]]())

  private def incomingDependencies(root : AGNode[Kind], acc0 : Set[AGEdge[Kind]]) : Set[AGEdge[Kind]]= {
    val acc1 = users.foldLeft(acc0){
      (acc, user) =>
        if(root contains_* user) acc
        else acc + AGEdge.uses(user, this)
    }
    content.foldLeft(acc1){(acc, child) => child.incomingDependencies(root, acc)}
  }

  def incomingDependencies : Set[AGEdge[Kind]] = incomingDependencies(this, Set[AGEdge[Kind]]())


  private def internalDependencies(root : AGNode[Kind], acc0 : Set[AGEdge[Kind]]) : Set[AGEdge[Kind]]= {
    val acc1 = uses.foldLeft(acc0) {
      (acc, usee) =>
        if (root contains_* usee)
          acc + AGEdge.uses(this, usee)
        else acc
    }
    /* not necessary
        val acc2 = users.foldLeft(acc1){
          (acc, user) =>
            if(root contains_* user)
              acc + AGEdge.uses(user, this)
            else acc
        }
    */

    content.foldLeft(acc1){(acc, child) => child.internalDependencies(root, acc)}
  }

  def internalDependencies : Set[AGEdge[Kind]] = internalDependencies(this, Set[AGEdge[Kind]]())


  /*def provides(other : AGNode[Kind]) = {
    val these0 = Set[AGNode[Kind]]() ++ this.iterator
    val others0 = Set[AGNode[Kind]]() ++ other.iterator

    val these = these0 -- others0
    val others = others0 -- these0
    these.exists { t => others.exists(o => o uses t) }
  }*/

  def provides(other : AGNode[Kind]) = {
    val these = Set[AGNode[Kind]]() ++ this.iterator
    val others = Set[AGNode[Kind]]() ++ other.iterator

    these.exists { t => others.exists(o => (o uses t) && !(other.contains_*(o) && other.contains_*(t)) ) }
  }

  private def connection( f : AGNode[Kind] => Boolean) = {
    graph.foldLeft(Set[AGNode[Kind]]()){ (acc, n) =>
      if(n == this || n.kind != this.kind) acc
      else if(f(n)) acc + n
      else acc
    }
  }

  def providers : Set[AGNode[Kind]] = connection{ n => n provides this}
  def clients : Set[AGNode[Kind]] = connection{ n => this provides n}

  def cohesion : Double = {
    val intd = internalDependencies.size
    intd.toDouble / (outgoingDependencies.size + incomingDependencies.size + intd).toDouble
  }

  def coupling : Double = {
    val dependencies = outgoingDependencies.size + incomingDependencies.size + internalDependencies.size
    1 - (providers ++ clients).size.toDouble / dependencies.toDouble
  }

  /*def outGoingDependencies : NodeSet[Kind] = {
    val ns = LiteralNodeSet[Kind](uses())
    content.foreach{ n => ns ++= n.outGoingDependencies}
    ns
  }*/
}



