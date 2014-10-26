package puck.graph

import puck.util.{PuckLog, HasChildren, BreadthFirstTreeIterator}

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

  import graph.{defaulVerbosity, logVerbosity}

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

  override def toString: String = (containerPath map (_.id)).mkString(".")
  //override def toString: String = id.toString
  //override def toString: String = "%d %s (%s)".format(id, kind, fullName)

  def nameTypeString = name + (kind match{case k : HasType[_, _] => " : " + k.typ; case _ => ""})

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

  def root() : NodeType = {
    if(isRoot) this
    else container.root()
  }

  def canContain(n : NodeType) : Boolean = {
    n != this && !(n contains_* this) && // no cycle !
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
    graph.logger.writeln("moving " + this +" from " + container + " to " + newContainer)

    AGEdge.contains(container, this).changeSource(newContainer)

    users.foreach{ user =>
      graph.redirectPrimaryUses(AGEdge.uses(user,this), this,
        Move(), propagateRedirection = false)
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
    other.directSuperTypes.exists(_ == this) ||
      other.directSuperTypes.exists(_.isSuperTypeOf(this))
  }

  def directSuperTypes : Iterable[NodeType] = superTypes0

  def superTypes() : Set[NodeType] =
    superTypes0.foldLeft(superTypes0.toSet){
      case (acc, n) => acc ++ n.superTypes()
    }

  private val subTypes0 : mutable.Set[NodeType] = mutable.Set()
  def directSubTypes : Iterable[NodeType] = subTypes0

  def subTypes(): Set[NodeType] =
    subTypes0.foldLeft(subTypes0.toSet){
      case (acc, n) => acc ++ n.subTypes()
    }

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
    directSuperTypes.exists( n => n == other)
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

  def abstractionCreationPostTreatment(abstraction : NodeType, policy : AbstractionPolicy){}

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



  def findMergingCandidate() : Option[AGNode[Kind]] = None

  /*def filterMergingCandidate(acceptEmptyContent : Boolean = true,
                             rootSearch : AGNode[Kind] = graph.root,
                             typeComparator : () => Boolean = () => true) : List[NodeType] = {

    if(this.content.isEmpty && acceptEmptyContent ||
      this.content.nonEmpty){
      rootSearch.iterator.filter{ n =>
        n != this && n.kind == this.kind &&
          {
            val sameSize = n.content.size == this.content.size


            if(sameSize)
              graph.logger.writeln("comparing %s with %s".format(this.name, n.name), 8)
            sameSize &&
              this.content.forall { child =>

                def typeC(other : HasType[Kind, _]) = {
                  child match {
                    case typedChild : HasType[Kind, _] =>
                      typedChild.`type`.copyWith(this).replacedBy(n) == other
                    case _ => false
                  }
                }


                child.searchMergingCandidate(rootSearch = this)

                child.kind match {
                  case absMethKind @ AbstractMethod() =>
                    absMethKind.findMergingCandidate(n) match {
                      case None => false
                      case Some(_) => true
                    }
                  case _ => throw new AGError("Interface should contain only abstract method !!")
                }
              }
          }

      }
    }
    else
        List()
  }*/

  //TODO deep merge : merge also content need to refactor find merging candidate
  //(deep merge is now done in JavaNode for interface node only)
  def mergeWith(other : NodeType){

    other.users().toList foreach { user =>
      AGEdge.uses(user, other).changeTarget(this)
      user.kind match {
        case k : HasType[Kind, _] => k.redirectUses(other, this)
        case _ => ()
      }
    }

    other.uses().toList foreach { AGEdge.uses(other, _).changeSource(this) }

    other.directSuperTypes.toList foreach {st =>
      if(st != this) AGEdge.isa(other, st).changeSource(this)
      else AGEdge.isa(other, st).delete()
    }

    other.directSubTypes.toList foreach { st =>
      if(st != this) AGEdge.isa(st, other).changeTarget(this)
      else AGEdge.isa(st, other).delete()
    }

    /*(this, key) is a primary uses and sidesUses(key) are the corresponding side uses */
    //val sideUses = new UsesDependencyMap(this, Dominant())

    /*(other, key) is a side uses and primaryUses(key) is the corresponding primary uses */
    //val primaryUses = new UsesDependencyMap(this, Dominated())


    val side_prim_list = graph.primaryUses.filter { case (e, _) => e.user == other }.toList

    side_prim_list foreach {
      case (AGEdge(Uses(), _, sideUsee), primUses) =>
        primUses.foreach { pUse =>
          graph.addUsesDependency(pUse, AGEdge.uses(this, sideUsee))
          graph.removeUsesDependency(pUse, AGEdge.uses(other, sideUsee))
        }
    }

    val prim_side_list = graph.sideUses.filter { case (e, _) => e.user == other }.toList

    prim_side_list foreach {
      case (AGEdge(Uses(), _, primeUsee), sidUses) =>
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



