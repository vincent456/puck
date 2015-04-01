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
  type EdgeMap = SetValueMap[NodeId, NodeId]
  val EdgeMap = SetValueMap
  type UseDependencyMap = SetValueMap[(NodeId,NodeId), (NodeId,NodeId)]
  val UseDependencyMap = SetValueMap
  type AbstractionMap = SetValueMap[NodeId, (NodeId, AbstractionPolicy)]
  val AbstractionMap = SetValueMap
  type Node2NodeMap = Map[NodeId, NodeId]
  val Node2NodeMap = Map

  type Nodes2VnodeMap = Map[Seq[NodeId], NodeId]
  val Nodes2VNodeMap = Map

  //type NodeT = (NodeId, String, NodeKind, TypeHolder, Mutability)
  type NodeT = DGNode
  type ConcreteNodeIndex = Map[NodeId, ConcreteNode]
  val ConcreteNodeIndex = Map
  type VirtualNodeIndex = Map[NodeId, VirtualNode]
  val VirtualNodeINdex = Map
  
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
  val nodeKindKnowledge: NodeKindKnowledge,
  private [this] val idSeed : Int,
  private [this] val nodesIndex : ConcreteNodeIndex,
  private [this] val removedNodes : ConcreteNodeIndex,
  private [this] val vNodesIndex : VirtualNodeIndex,
  private [this] val vRemovedNodes : VirtualNodeIndex,
  private [this] val nodes2vNodes : Nodes2VnodeMap,
  private [this] val usersMap : EdgeMap,
  private [this] val usesMap  : EdgeMap,
  private [this] val contentsMap  : EdgeMap,
  private [this] val containerMap : Node2NodeMap,
  private [this] val superTypesMap : EdgeMap,
  private [this] val subTypesMap : EdgeMap,
  //TODO remake private [this]
  /*private [this]*/ val typeMemberUses2typeUsesMap : UseDependencyMap,
  /*private [this]*/ val typeUses2typeMemberUsesMap : UseDependencyMap,
  private [this] val abstractionsMap : AbstractionMap,
  val constraints : ConstraintsMaps,
  val recording : transformations.Recording) {


  type EdgeT = DGEdge
  type GraphT = DependencyGraph

  def newGraph(nLogger : PuckLogger = logger,
               nIdSeed : Int = idSeed,
               nNodesSet : ConcreteNodeIndex = nodesIndex,
               nRemovedNodes : ConcreteNodeIndex = removedNodes,
               nVNodesIndex : VirtualNodeIndex = vNodesIndex,
               nVRemovedNodes : VirtualNodeIndex = vRemovedNodes,
               nNodes2vNodes : Nodes2VnodeMap = nodes2vNodes,
               nUsersMap : EdgeMap = usersMap,
               nUsesMap  : EdgeMap = usesMap,
               nContentMap  : EdgeMap = contentsMap,
               nContainerMap : Node2NodeMap = containerMap,
               nSuperTypesMap : EdgeMap = superTypesMap,
               nSubTypesMap : EdgeMap = subTypesMap,
               nDominantUsesMap : UseDependencyMap = typeMemberUses2typeUsesMap,
               nDominatedUsesMap : UseDependencyMap = typeUses2typeMemberUsesMap,
               nAbstractionsMap : AbstractionMap = abstractionsMap,
               nConstraints : ConstraintsMaps = constraints,
               nRecording : transformations.Recording = recording) : DependencyGraph =
    new DependencyGraph(nLogger, nodeKindKnowledge, nIdSeed,
                        nNodesSet, nRemovedNodes, nVNodesIndex, nVRemovedNodes, nNodes2vNodes,
                        nUsersMap, nUsesMap,
                        nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
                        nDominantUsesMap, nDominatedUsesMap,
                        nAbstractionsMap, nConstraints, nRecording)

  def withLogger(l : PuckLogger) = newGraph(nLogger = l)
  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.InGraph, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.InGraph, lvl)


  val rootId : NodeId = 0
  def root = getNode(rootId)
  def isRoot(id : NodeId) = id == rootId

   private [graph] def addConcreteNode(n : ConcreteNode) : DependencyGraph =
     newGraph(nNodesSet = nodesIndex + (n.id -> n),
       nRemovedNodes = removedNodes - n.id,
       nRecording = recording.addConcreteNode(n))

  def addConcreteNode(localName:String, kind: NodeKind, th : Option[Type],
                      mutable : Mutability = true, sid : Option[NodeId] = None) : (ConcreteNode, GraphT) = {
    val (id, g) = sid match {
      case None => (idSeed + 1, newGraph(nIdSeed = idSeed + 1))
      case Some(i) => (i, this)
    }
    val n = ConcreteNode(id, localName, kind, th, mutable)
    (n, g.addConcreteNode(n))
  }

  private def addVirtualNode(n : VirtualNode) : DependencyGraph =
    newGraph(nVNodesIndex = vNodesIndex + (n.id -> n),
      nVRemovedNodes = vRemovedNodes - n.id,
      nNodes2vNodes = nodes2vNodes + (n.potentialMatches -> n.id),
      nRecording = recording.addVirtualNode(n))


  def addVirtualNode(ns : Seq[NodeId], k : NodeKind) : (VirtualNode, GraphT) = {
    nodes2vNodes get ns match {
      case Some(vnid) => (vNodesIndex(vnid), this)
      case None => val n = VirtualNode(idSeed + 1, ns, k)
        (n, newGraph(nIdSeed = idSeed + 1).addVirtualNode(n))
    }
  }

  def nodes : Iterable[DGNode] = nodesIndex.values ++ vNodesIndex.values
  def concreteNodes : Iterable[ConcreteNode] = nodesIndex.values
  def virtualNodes : Iterable[VirtualNode] = vNodesIndex.values
  /*map {
    case (nid, name, kind, styp, mutable) => ConcreteNode(nid, name, kind, styp, mutable, Created)
  }*/


  def nodesId : Iterable[NodeId] = nodesIndex.keys ++ vNodesIndex.keys
  def concreteNodesId : Iterable[NodeId] = nodesIndex.keys
  def numNodes : Int = nodesIndex.size + vNodesIndex.size

  def isaEdges : List[DGEdge] = superTypesMap.content.foldLeft(List[DGEdge]()){
    case (acc, (sub, sups)) =>
      sups.toList.map(sup => DGEdge.isa(sub, sup)) ::: acc
  }

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


  def getConcreteNodeWithStatus(id : NodeId): (ConcreteNode, NodeStatus) =
    nodesIndex get id map {(_, Created)} getOrElse{
      removedNodes get id map {(_, Removed)} getOrElse {
        val msg =
          s"AccessGraph.getNode : no concrete node has id ${id.toString}\n" +
            s"concrete nodes :${nodesIndex.toSeq.sortBy(_._1).map(_._2).mkString("\n\t", "\n\t", "\n")}" +
            s"virtual nodes :${vNodesIndex.toSeq.sortBy(_._1).map(_._2).mkString("\n\t", "\n\t", "")}"
        throw new DGError(msg)
      }
    }


  def getNode(id : NodeId): DGNode = try {
    getConcreteNodeWithStatus(id)._1
  } catch {
    case e : DGError => vNodesIndex(id)
  }
  def getConcreteNode(id : NodeId): ConcreteNode = getConcreteNodeWithStatus(id)._1


  def removeConcreteNode(id : NodeId) = {
    val n = nodesIndex(id)
    if(n.id != id)
      throw new DGError("incoherent index left and right id are different")
    newGraph(nNodesSet = nodesIndex - id,
      nRemovedNodes = removedNodes + (id -> n),
      nRecording = recording removeConcreteNode n)
   /* match {
      case node @ (`id`, localName, kind, styp, mutable) =>
        newGraph(nNodesSet = nodesIndex - id,
          nRemovedNodes = removedNodes + (id -> node),
          nRecording = recording.removeNode(id, localName, kind, styp, mutable))
      case _ => throw new DGError("incoherent index left and right id are different")

    }*/
  }

  def removeVirtualNode(id : NodeId) = {
    val n = vNodesIndex(id)
    if(n.id != id)
      throw new DGError("incoherent index left and right id are different")
    newGraph(nVNodesIndex = vNodesIndex + (n.id -> n),
      nVRemovedNodes = vRemovedNodes - n.id,
      nNodes2vNodes = nodes2vNodes + (n.potentialMatches -> n.id),
      nRecording = recording.addVirtualNode(n))
  }


  private def setNode(n : DGNode, s : NodeStatus) : GraphT = (n, s) match {
      case (cn: ConcreteNode, Created) =>
        newGraph(nNodesSet = nodesIndex + (n.id -> cn) )
      case (cn : ConcreteNode, Removed) =>
        newGraph(nRemovedNodes = removedNodes + (n.id -> cn) )
      case (vn: VirtualNode, Created) =>
        newGraph(nVNodesIndex = vNodesIndex + (n.id -> vn) )
      case (vn : VirtualNode, Removed) =>
        newGraph(nVRemovedNodes = vRemovedNodes + (n.id -> vn) )
  }

  def setType(id : NodeId, st : Option[Type]) : GraphT = {
    val (n, s) = getConcreteNodeWithStatus(id)
    setNode(n.copy(styp = st), s)
  }


  def setMutability(id : NodeId, mutable : Boolean) =  {
    val (n, s) = getConcreteNodeWithStatus(id)
    setNode(n.copy(isMutable = mutable), s)
  }


 def addContains(containerId: NodeId, contentId :NodeId, register : Boolean = true): GraphT =
     newGraph(nContentMap = contentsMap + (containerId, contentId),
              nContainerMap = containerMap + (contentId -> containerId),
              nRecording = if(register) recording.addEdge(DGEdge.contains(containerId, contentId))
                           else recording)

  def removeContains(containerId: NodeId, contentId :NodeId, register : Boolean = true): GraphT =
      newGraph( nContentMap = contentsMap - (containerId, contentId),
                nContainerMap = containerMap - contentId,
                nRecording = if(register) recording.removeEdge(DGEdge.contains(containerId, contentId))
                             else recording )

  def addUses(userId: NodeId, useeId: NodeId, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap + (useeId , userId),
             nUsesMap = usesMap + (userId, useeId),
             nRecording = if(register) recording.addEdge(DGEdge.uses(userId, useeId))
                          else recording)

  def removeUses(userId: NodeId, useeId: NodeId, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap - (useeId , userId),
             nUsesMap = usesMap - (userId, useeId),
             nRecording = if(register) recording.removeEdge(DGEdge.uses(userId, useeId))
                          else recording)

  def addIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap + (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap + (subTypeId, superTypeId),
            nRecording = if(register) recording.addEdge(DGEdge.isa(subTypeId,superTypeId))
                         else recording)

  def removeIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap - (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap - (subTypeId, superTypeId),
             nRecording = if(register) recording.removeEdge(DGEdge.isa(subTypeId,superTypeId))
                          else recording)


  def addUsesDependency(dominantEdge : (NodeId, NodeId),
                        dominatedEdge : (NodeId, NodeId)) : GraphT =
    newGraph(nDominantUsesMap = typeMemberUses2typeUsesMap + (dominatedEdge, dominantEdge),
      nDominatedUsesMap = typeUses2typeMemberUsesMap + (dominantEdge, dominatedEdge))

  def removeUsesDependency(dominantEdge : EdgeT,
                           dominatedEdge :EdgeT) : GraphT =
    removeUsesDependency((dominantEdge.source, dominantEdge.target),
      (dominatedEdge.source, dominatedEdge.target))

  def removeUsesDependency(dominantEdge : (NodeId, NodeId),
                           dominatedEdge : (NodeId, NodeId)) : GraphT =
    newGraph(nDominantUsesMap = typeMemberUses2typeUsesMap - (dominatedEdge, dominantEdge),
      nDominatedUsesMap = typeUses2typeMemberUsesMap - (dominantEdge, dominatedEdge))

  def addAbstraction(id : NodeId, abs : (NodeId, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap + (id, abs),
             nRecording = recording.addAbstraction(id, abs._1, abs._2))

  def removeAbstraction(id : NodeId, abs : (NodeId, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap - (id, abs),
             nRecording = recording.removeAbstraction(id, abs._1, abs._2))

  def changeTarget(edge : EdgeT, newTarget : NodeId) : GraphT = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge : EdgeT = new DGEdge(edge.kind, edge.source, newTarget)
    val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = newEdge.existsIn(this))
    newEdge.createIn(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeSource(edge : EdgeT, newSource : NodeId) : GraphT = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge: EdgeT = new DGEdge(edge.kind, newSource, edge.target)
    val newRecording = recording.changeEdgeSource(edge, newSource, withMerge = newEdge.existsIn(this))
    newEdge.createIn(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeType(id : NodeId, styp : Option[Type], oldUsee: NodeId, newUsee : NodeId) : GraphT =
    styp match {
      case None => this
      case Some(t) => val newTyp= Some(t.redirectUses(oldUsee, getNode(newUsee)))
        setType(id, newTyp).
          newGraph(nRecording = recording.addTypeChange(id, styp, oldUsee, newUsee))
    }

  def changeContravariantType(id : NodeId, styp : Option[Type], oldUsee: NodeId, newUsee : NodeId) : GraphT =
  styp match {
    case None => this
    case Some(t) => val newTyp= Some(t.redirectContravariantUses(oldUsee, getNode(newUsee)))
      setType(id, newTyp).
        newGraph(nRecording = recording.addTypeChange(id, styp, oldUsee, newUsee))
  }

  /*
   * Read-only queries
   */

  def nodeKinds : Seq[NodeKind] = nodeKindKnowledge.nodeKinds

  def container(contentId : NodeId) : Option[NodeId] = containerMap.get(contentId)
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

  def content(containerId: NodeId) : Set[NodeId] = contentsMap.getFlat(containerId)

  def contains(containerId : NodeId, contentId : NodeId) : Boolean =
    container(contentId) match {
      case None => false
      case Some(id) => id == containerId
    }

  def contains_*(containerId : NodeId, contentId : NodeId) : Boolean =
    containerId == contentId || {
      container(contentId) match {
        case None => false
        case Some(id) =>
          //import ShowDG._
          //logger.writeln(ShowDG.showDG[NodeId](this).show(id))(PuckLog.Debug)
          contains_*(containerId, id)
      }
    }

  def canContain(n : DGNode, cn : ConcreteNode) : Boolean =
      nodeKindKnowledge.canContain(this)(n,cn)

  def containerPath(id : NodeId)  : Seq[NodeId] = {
    def aux(current : NodeId, acc : Seq[NodeId]) : Seq[NodeId] = {
      val cter = container(current)
      if (cter.isEmpty) current +: acc
      else aux(cter.get, current +: acc)
    }

    aux(id, Seq())
  }

  def fullName(id : NodeId) : String = {
    /*if (isRoot) nameTypeString
      else {*/
    import ShowDG._
    val path = containerPath(id).map{n => showDG[DGNode](this)(nodeNameTypCord).shows(getNode(n))}

    (if (path.head == DependencyGraph.rootName)
      path.tail
    else
      DependencyGraph.unrootedStringId +: path ).mkString(DependencyGraph.scopeSeparator)
  }


  def directSuperTypes(sub: NodeId) : Iterable[NodeId] = superTypesMap getFlat sub
  def directSubTypes(sup: NodeId) : Iterable[NodeId] = subTypesMap getFlat sup

  def subTypes(sup : NodeId) : Iterable[NodeId]= {
    val dst = directSubTypes(sup).toSeq
    dst.foldLeft(dst) { case (acc, id) => acc ++ subTypes(id) }
  }

  def isSuperTypeOf(superCandidate: NodeId, subCandidate : NodeId) : Boolean = {
    directSuperTypes(subCandidate).exists(_ == superCandidate) ||
      directSuperTypes(subCandidate).exists(isSuperTypeOf(superCandidate, _))
  }

  def isa(subId : NodeId, superId: NodeId): Boolean = superTypesMap.bind(subId, superId)
  
  def isaSeq  : Seq[(NodeId, NodeId)] = superTypesMap.flatSeq
  
  def uses(userId: NodeId, usedId: NodeId) : Boolean = usersMap.bind(usedId, userId)

  def usesSeq : Seq[(NodeId, NodeId)] = usesMap.flatSeq
  
  def usedBy(userId : NodeId) : Iterable[NodeId] = usesMap getFlat userId
  
  def users(useeId: NodeId) : Iterable[NodeId] = usersMap getFlat useeId

  def typeUsesOf(typeMemberUse : (NodeId, NodeId)) : Iterable[(NodeId, NodeId)] =
    typeMemberUses2typeUsesMap getFlat typeMemberUse

  def typeMemberUsesOf(typeUse : (NodeId, NodeId)) : Iterable[(NodeId, NodeId)] =
    typeUses2typeMemberUsesMap  getFlat typeUse

  def dominates(dominantEdge : (NodeId, NodeId),
                dominatedEdge : (NodeId, NodeId)) : Boolean =
    typeMemberUsesOf( dominantEdge ).exists(_ == dominatedEdge)

  def abstractions(id : NodeId) : Iterable[(NodeId, AbstractionPolicy)] =
    abstractionsMap getFlat id

  def violations() : Seq[EdgeT] =
    concreteNodesId.flatMap { n =>
      val wu = constraints.wrongUsers(this, n).map(DGEdge.uses(_,n))
      if(constraints.isWronglyContained(this, n))
         DGEdge.contains(container(n).get, n) +: wu
      else wu
    }.toSeq

  def isViolation(e : DGEdge) : Boolean = {
    e.kind match {
      case Contains =>
        constraints.isWronglyContained(this, e.target)
      case Uses | Isa =>
        constraints.violation(this, e.user, e.used)

    }
  }

  def wrongUsers(id : NodeId) : Seq[NodeId] = constraints.wrongUsers(this, id)
  def isWronglyContained(id : NodeId) = constraints.isWronglyContained(this, id)
  def interloperOf(id1 : NodeId, id2 :NodeId) = constraints.violation(this, id1, id2)

  def printConstraints[V](logger : Logger[V], v : V) : Unit =
    constraints.printConstraints(this, logger, v)

 def coupling = nodeKindKnowledge.coupling(this)

  def subTree(root : NodeId, includeRoot : Boolean = true) : Seq[NodeId] = {
    type Roots = Seq[NodeId]
    def aux(acc : Seq[NodeId]) : Roots => Seq[NodeId] = {
      case Seq() => acc
      case r +: tail =>
        val children = content(r)
        aux(children ++: acc)(children ++: tail)

    }

    val seqInit =
      if(includeRoot) Seq(root)
      else Seq()

    aux(seqInit)(Seq(root))
  }

  def kindType(nid : NodeId) : KindType =
    kindType(getNode(nid))

  def kindType(n : DGNode) : KindType =
    nodeKindKnowledge.kindType(this, n)
}