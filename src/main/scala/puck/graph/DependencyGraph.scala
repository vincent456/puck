package puck.graph

import puck.graph.DependencyGraph._
import puck.graph.constraints._
import puck.graph.transformations.RecordingComparator
import puck.util.{Logger, PuckNoopLogger, PuckLogger, PuckLog}

/**
 * Created by lorilan on 1/8/15.
 */

sealed trait NodeStatus
case object Removed extends NodeStatus
case object Created extends NodeStatus

object DependencyGraph {

  val rootId : NodeId = 0
  val dummyId = Int.MinValue
  /*val dummyNamedType = NamedType(0, "DummyType")
  val dummyArrowType = Arrow(dummyNamedType, dummyNamedType)*/
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"
  val scopeSeparator : String = "."

  //phantom type of NodeId used to ease Mutable/Immutable transition
  //in mutable NodeId[K] =:= AGNode[K]
  type NodeId = Int
  type EdgeMap = SetValueMap[NodeId, NodeId]
  val EdgeMap = SetValueMap
  type UseDependencyMap = SetValueMap[(NodeId,NodeId), (NodeId,NodeId)]
  val UseDependencyMap = SetValueMap
  type AbstractionMap = SetValueMap[NodeId, (NodeId, AbstractionPolicy)]
  val AbstractionMap = SetValueMap
  type Node2NodeMap = Map[NodeId, NodeId]
  val Node2NodeMap = Map

  //type NodeT = (NodeId, String, NodeKind, TypeHolder, Mutability)
  type NodeT = DGNode
  type NodeIndex = Map[NodeId, NodeT]
  val NodeIndex = Map

  implicit def idToNode(implicit graph : DependencyGraph, id : NodeId) : DGNode =
               graph.getNode(id)

  type Mutability = Boolean

  def areEquivalent[Kind <: NodeKind, T](initialRecord : transformations.Recording,
                      graph1 : DependencyGraph,
                      graph2 : DependencyGraph,
                      logger : PuckLogger = PuckNoopLogger) : Boolean = {
    val engine = new RecordingComparator(initialRecord(),graph1,graph2, logger)
    engine.explore()
    engine.finalStates.nonEmpty
  }

}

class DependencyGraph
( val logger : PuckLogger = PuckNoopLogger,
  private [this] val idSeed : () => Int,
  private [this] val nodesIndex : NodeIndex,
  private [this] val removedNodes : NodeIndex,
  private [this] val usersMap : EdgeMap,
  private [this] val usesMap  : EdgeMap,
  private [this] val contentsMap  : EdgeMap,
  private [this] val containerMap : Node2NodeMap,
  private [this] val superTypesMap : EdgeMap,
  private [this] val subTypesMap : EdgeMap,
  //TODO remake private [this]
  /*private [this]*/ val memberUses2typeUsesMap : UseDependencyMap,
  /*private [this]*/ val typeUses2memberUsesMap : UseDependencyMap,
  private [this] val abstractionsMap : AbstractionMap,
  val constraints : ConstraintsMaps,
  val recording : transformations.Recording) {


  type NIdT = NodeId
  type EdgeT = DGEdge
  type GraphT = DependencyGraph
  type STyp = TypeHolder

  def newGraph(nLogger : PuckLogger = logger,
               nNodesSet : NodeIndex = nodesIndex,
               nRemovedNodes : NodeIndex = removedNodes,
               nUsersMap : EdgeMap = usersMap,
               nUsesMap  : EdgeMap = usesMap,
               nContentMap  : EdgeMap = contentsMap,
               nContainerMap : Node2NodeMap = containerMap,
               nSuperTypesMap : EdgeMap = superTypesMap,
               nSubTypesMap : EdgeMap = subTypesMap,
               nDominantUsesMap : UseDependencyMap = memberUses2typeUsesMap,
               nDominatedUsesMap : UseDependencyMap = typeUses2memberUsesMap,
               nAbstractionsMap : AbstractionMap = abstractionsMap,
               nConstraints : ConstraintsMaps = constraints,
               nRecording : transformations.Recording = recording) : DependencyGraph =
    new DependencyGraph(nLogger,
                        idSeed,
                        nNodesSet, nRemovedNodes, nUsersMap, nUsesMap,
                        nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
                        nDominantUsesMap, nDominatedUsesMap,
                        nAbstractionsMap, nConstraints, nRecording)

  def withLogger(l : PuckLogger) = newGraph(nLogger = l)
  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.InGraph, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.InGraph, lvl)


  private [graph] def addNode(id : NIdT,
                              localName:String,
                              kind: NodeKind,
                              styp: STyp,
                              mutable : Mutability) : DependencyGraph =
    newGraph(nNodesSet = nodesIndex + (id -> DGNode(id, localName, kind, styp, mutable, Created)),
             nRemovedNodes = removedNodes - id,
             nRecording = recording.addNode(id, localName, kind, styp, mutable))



  val rootId : NIdT = 0
  def root = getNode(rootId)
  def isRoot(id : NIdT) = id == rootId

  def addNode(localName:String, kind: NodeKind, th : TypeHolder, mutable : Mutability = true) : (NIdT, GraphT) = {
    val id = idSeed()
    (id, addNode(id, localName, kind, th, mutable))
  }

  def nodes : Iterable[DGNode] = nodesIndex.values /*map {
    case (nid, name, kind, styp, mutable) => ConcreteNode(nid, name, kind, styp, mutable, Created)
  }*/

  private def sortedMap : Seq[(NIdT,  NodeT)] = nodesIndex.toSeq sortBy(_._1)

  def nodesId : Iterable[NodeId] = nodesIndex.keys
  def numNodes : Int = nodesIndex.size
  
  /*def getNodeTuple(id : NIdT) = nodesIndex get id match {
    case Some((_, name, kind, styp, mutable)) => (id, name, kind, styp, mutable, Created)
    case None => removedNodes get id match {
      case Some((_, name, kind, styp, mutable)) => (id, name, kind, styp, mutable, Removed)
      case None =>
        val msg = "AccessGraph.getNode : no node has id " + id.toString
        logger.writeln(msg)(PuckLog.Error)
        logger.writeln("nodes of graph : ")(PuckLog.Error)
        logger.writeln(sortedMap.mkString("\n", "\n\t", ""))(PuckLog.Error)
        throw new DGError("illegal node request : no node has id " + id.toString)
    }
  }

  def getNode(id : NIdT): DGNode = {
    val t = getNodeTuple(id)
    DGNode(t._1, t._2, t._3, t._4, t._5, t._6)
  }*/


  def getNode(id : NIdT): DGNode = nodesIndex get id match {
    case Some(n) => n
    case None => removedNodes get id match {
      case Some(n) => n
      case None =>
        val msg = "AccessGraph.getNode : no node has id " + id.toString
        logger.writeln(msg)(PuckLog.Error)
        logger.writeln("nodes of graph : ")(PuckLog.Error)
        logger.writeln(sortedMap.mkString("\n", "\n\t", ""))(PuckLog.Error)
        throw new DGError("illegal node request : no node has id " + id.toString)
    }
  }

  def removeNode(id : NIdT) = {
    val n = nodesIndex(id)
    if(n.id != id)
      throw new DGError("incoherent index left and right id are different")
    newGraph(nNodesSet = nodesIndex - id,
      nRemovedNodes = removedNodes + (id -> n),
      nRecording = recording.removeNode(id, n.name, n.kind, n.styp, n.isMutable))
   /* match {
      case node @ (`id`, localName, kind, styp, mutable) =>
        newGraph(nNodesSet = nodesIndex - id,
          nRemovedNodes = removedNodes + (id -> node),
          nRecording = recording.removeNode(id, localName, kind, styp, mutable))
      case _ => throw new DGError("incoherent index left and right id are different")

    }*/
  }



  def setNode(n : DGNode) : GraphT = n.status match {
      case Created => newGraph(nNodesSet = nodesIndex + (n.id -> n) )
      case Removed => newGraph(nRemovedNodes = removedNodes + (n.id -> n) )
  }

  //def setNode(n : DGNode) : GraphT = setNode(n.id, n.name, n.kind, n.styp, n.isMutable, n.status)
  //def setNode(n : NodeT, status : NodeStatus) : GraphT = setNode(n._1, n._2, n._3, n._4, n._5, status)
  /*def setNode(id : NIdT, name : String, k : NodeKind, styp : STyp, mutable : Boolean, t : NodeStatus) : GraphT = {
    val assoc = id -> DGNode(id, name, k, styp, mutable, t)
    t match {
      case Created => newGraph(nNodesSet = nodesIndex + assoc )
      case Removed => newGraph(nRemovedNodes = removedNodes + assoc )
    }
  }*/

/*  def setKind(id : NIdT, k : NodeKind) = getNodeTuple(id) match {
    case (_, name, _, styp, mutability, status) => setNode(id, name, k, styp, mutability, status)
  }

  def setType(id : NIdT, st : STyp) =
    getNodeTuple(id) match {
      case (_, name, k, _, mutability, t) =>  setNode(id, name, k, st, mutability, t)
    }

 def setMutability(id : NIdT, mutable : Boolean) =
   getNodeTuple(id) match {
     case (_, name, k, st, _, t) =>  setNode(id, name, k, st, mutable, t)
   }*/


  def setType(id : NIdT, st : STyp) : GraphT = {
    val n = getNode(id) match {
      case cn : ConcreteNode => cn.copy(styp = st)
      case _ => throw new DGError("set type unhandled case")
    }
    setNode(n)
  }

  def setMutability(id : NIdT, mutable : Boolean) = {
      val n = getNode(id) match {
      case cn : ConcreteNode => cn.copy(isMutable = mutable)
      case _ => throw new DGError("set type unhandled case")
    }
    setNode(n)
  }

 def addContains(containerId: NIdT, contentId :NIdT, register : Boolean = true): GraphT =
     newGraph(nContentMap = contentsMap + (containerId, contentId),
              nContainerMap = containerMap + (contentId -> containerId),
              nRecording = if(register) recording.addEdge(DGEdge.contains(containerId, contentId))
                           else recording)

  def removeContains(containerId: NIdT, contentId :NIdT, register : Boolean = true): GraphT =
      newGraph( nContentMap = contentsMap - (containerId, contentId),
                nContainerMap = containerMap - contentId,
                nRecording = if(register) recording.removeEdge(DGEdge.contains(containerId, contentId))
                             else recording )

  def addUses(userId: NIdT, useeId: NIdT, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap + (useeId , userId),
             nUsesMap = usesMap + (userId, useeId),
             nRecording = if(register) recording.addEdge(DGEdge.uses(userId, useeId))
                          else recording)

  def removeUses(userId: NIdT, useeId: NIdT, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap - (useeId , userId),
             nUsesMap = usesMap - (userId, useeId),
             nRecording = if(register) recording.removeEdge(DGEdge.uses(userId, useeId))
                          else recording)

  def addIsa(subTypeId: NIdT, superTypeId: NIdT, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap + (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap + (subTypeId, superTypeId),
            nRecording = if(register) recording.addEdge(DGEdge.isa(subTypeId,superTypeId))
                         else recording)

  def removeIsa(subTypeId: NIdT, superTypeId: NIdT, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap - (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap - (subTypeId, superTypeId),
             nRecording = if(register) recording.removeEdge(DGEdge.isa(subTypeId,superTypeId))
                          else recording)


  def addUsesDependency(dominantEdge : (NIdT, NIdT),
                        dominatedEdge : (NIdT, NIdT)) : GraphT =
    newGraph(nDominantUsesMap = memberUses2typeUsesMap + (dominatedEdge, dominantEdge),
      nDominatedUsesMap = typeUses2memberUsesMap + (dominantEdge, dominatedEdge))

  def removeUsesDependency(dominantEdge : EdgeT,
                           dominatedEdge :EdgeT) : GraphT =
    removeUsesDependency((dominantEdge.source, dominantEdge.target),
      (dominatedEdge.source, dominatedEdge.target))

  def removeUsesDependency(dominantEdge : (NIdT, NIdT),
                           dominatedEdge : (NIdT, NIdT)) : GraphT =
    newGraph(nDominantUsesMap = memberUses2typeUsesMap - (dominatedEdge, dominantEdge),
      nDominatedUsesMap = typeUses2memberUsesMap - (dominantEdge, dominatedEdge))

  def addAbstraction(id : NIdT, abs : (NIdT, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap + (id, abs),
             nRecording = recording.addAbstraction(id, abs._1, abs._2))

  def removeAbstraction(id : NIdT, abs : (NIdT, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap - (id, abs),
             nRecording = recording.removeAbstraction(id, abs._1, abs._2))

  /*def addEdge(edge : EdgeType, register : Boolean = true) : AccessGraph = edge.kind match {
    case Uses() => addUses(edge.user, edge.usee, register)
    case Contains() => addContains(edge.source, edge.target, register)
    case Isa() => addIsa(edge.source, edge.target, register)
  }

  def removeEdge(edge : EdgeType, register : Boolean = true) : AccessGraph = edge.kind match {
    case Uses() => removeUses(edge.user, edge.usee, register)
    case Contains() => removeContains(edge.source, edge.target, register)
    case Isa() => removeIsa(edge.source, edge.target, register)
  }*/

  def changeTarget(edge : EdgeT, newTarget : NIdT) : GraphT = {
    val g1 = edge.delete(this, register = false)
    val newEdge : EdgeT = new DGEdge(edge.kind, edge.source, newTarget)
    val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = newEdge.exists(this))
    newEdge.create(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeSource(edge : EdgeT, newSource : NIdT) : GraphT = {
    val g1 = edge.delete(this, register = false)
    val newEdge: EdgeT = new DGEdge(edge.kind, newSource, edge.target)
    val newRecording = recording.changeEdgeSource(edge, newSource, withMerge = newEdge.exists(this))
    newEdge.create(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeType(id : NIdT, typ : STyp, oldUsee: NIdT, newUsee : NIdT) : GraphT = {
    val newTyp= typ.redirectUses(oldUsee, getNode(newUsee))

    setType(id, newTyp).
      newGraph(nRecording = recording.addTypeChange(id, typ, oldUsee, newUsee))

  }

  def changeContravariantType(id : NIdT, typ : STyp, oldUsee: NIdT, newUsee : NIdT) : GraphT = {
    val newTyp= typ.redirectContravariantUses(oldUsee, getNode(newUsee))

    setType(id, newTyp).
      newGraph(nRecording = recording.addTypeChange(id, typ, oldUsee, newUsee))

  }
  /*
   * Read-only queries
   */

  def nodeKinds : Seq[NodeKind] = Seq()

  def container(contentId : NIdT) : Option[NIdT] = containerMap.get(contentId)
    /*containerMap.get(contentId) match {
      case None => contentId
        /*val msg = "AccessGraph.container : no container for " + getNode(contentId)
        logger.writeln(msg)(PuckLog.Error)
        logger.writeln("nodes of graph : ")(PuckLog.Error)
        logger.writeln(sortedMap.mkString("\n", "\n\t", ""))(PuckLog.Error)
        logger.writeln("container map of graph : ")(PuckLog.Error)
        logger.writeln(containerMap.toSeq.sortBy(_._1).mkString("\n", "\n\t", ""))
        throw new AGError(msg)*/
      case Some(id) => id
    }*/



  def content(containerId: NIdT) : Iterable[NIdT] = contentsMap.getFlat(containerId)

  def contains(containerId : NIdT, contentId : NIdT) : Boolean =
    container(contentId) match {
      case None => false
      case Some(id) => id == containerId
    }

  def contains_*(containerId : NIdT, contentId : NIdT) : Boolean =
    containerId == contentId || {
      container(contentId) match {
        case None => false
        case Some(id) => contains_*(containerId, id)
      }
    }

  def canContain(id : NIdT, otherId : NIdT): Boolean = {
    val n = getNode(id)
    val other = getNode(otherId)
    !contains_*(otherId, id) && // no cycle !
      (n.kind canContain other.kind) &&
      n.isMutable
  }

  def containerPath(id : NIdT)  : Seq[NIdT] = {
    def aux(current : NIdT, acc : Seq[NIdT]) : Seq[NIdT] = {
      val cter = container(current)
      if (cter.isEmpty) current +: acc
      else aux(cter.get, current +: acc)
    }

    aux(id, Seq())
  }

  def fullName(id : NIdT) : String = {
    /*if (isRoot) nameTypeString
      else {*/
    import ShowDG._
    val path = containerPath(id).map{n => showDG[DGNode](this)(nodeNameTypCord).shows(getNode(n))}

    (if (path.head == DependencyGraph.rootName)
      path.tail
    else
      DependencyGraph.unrootedStringId +: path ).mkString(DependencyGraph.scopeSeparator)
  }


  def directSuperTypes(sub: NIdT) : Iterable[NIdT] = superTypesMap getFlat sub
  def directSubTypes(sup: NIdT) : Iterable[NIdT] = subTypesMap getFlat sup

  def subTypes(sup : NIdT) : Iterable[NIdT]= {
    val dst = directSubTypes(sup).toSeq
    dst.foldLeft(dst) { case (acc, id) => acc ++ subTypes(id) }
  }

  def isSuperTypeOf(superCandidate: NIdT, subCandidate : NIdT) : Boolean = {
    directSuperTypes(subCandidate).exists(_ == superCandidate) ||
      directSuperTypes(subCandidate).exists(isSuperTypeOf(superCandidate, _))
  }

  def isa(subId : NIdT, superId: NIdT): Boolean = superTypesMap.bind(subId, superId)

  def uses(userId: NIdT, useeId: NIdT) : Boolean = usersMap.bind(useeId, userId)

  def usedBy(userId : NIdT) : Iterable[NIdT] = usesMap getFlat userId
  
  def users(useeId: NIdT) : Iterable[NIdT] = usersMap getFlat useeId

  def typeUsesOf(typeMemberUse : (NIdT, NIdT)) : Iterable[(NIdT, NIdT)] =
    memberUses2typeUsesMap getFlat typeMemberUse

  def typeMemberUsesOf(typeUse : (NIdT, NIdT)) : Iterable[(NIdT, NIdT)] =
    typeUses2memberUsesMap  getFlat typeUse

  def dominates(dominantEdge : (NIdT, NIdT),
                dominatedEdge : (NIdT, NIdT)) : Boolean =
    typeMemberUsesOf( dominantEdge ).exists(_ == dominatedEdge)

  def abstractions(id : NIdT) : Iterable[(NIdT, AbstractionPolicy)] =
    abstractionsMap getFlat id

  def violations() : Seq[EdgeT] =
    nodesIndex.keys.flatMap {n =>
      val wu = constraints.wrongUsers(this, n).map(DGEdge.uses(_,n))
      if(constraints.isWronglyContained(this, n))
         DGEdge.contains(container(n).get, n) +: wu
      else wu
    }.toSeq

  def wrongUsers(id : NIdT) : Seq[NIdT] = constraints.wrongUsers(this, id)
  def isWronglyContained(id : NIdT) = constraints.isWronglyContained(this, id)
  def interloperOf(id1 : NIdT, id2 :NIdT) = constraints.interloperOf(this, id1, id2)

  def printConstraints[V](logger : Logger[V], v : V) : Unit =
    constraints.printConstraints(this, logger, v)

 def coupling = nodesIndex.keys.foldLeft(0 : Double){
   (acc, id) => acc + Metrics.coupling(id, this)
 }

  def subTree(root : NIdT) : Seq[NIdT] = {
    def aux(roots : Seq[NIdT], acc : Seq[NIdT]): Seq[NIdT] = roots match {
      case Seq() => acc
      case r +: tail =>
        val children = content(r)
        aux(children ++: tail, children ++: acc)

    }
    aux(Seq(root), Seq(root))
  }

  def isTypeUse : DGEdge => Boolean = _ => false
  def isTypeMemberUse : DGEdge => Boolean = _ => false
}