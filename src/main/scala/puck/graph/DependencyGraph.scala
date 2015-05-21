package puck.graph

import puck.graph.DependencyGraph._
import puck.graph.constraints._
import puck.graph.transformations.{Transformation, RecordingComparator}
import puck.util.{Logger, PuckNoopLogger, PuckLogger, PuckLog}

import scala.annotation.tailrec

sealed trait NodeStatus
case object Removed extends NodeStatus
case object Created extends NodeStatus

import transformations.RecordingOps

object DependencyGraph {

  val rootId : NodeId = 0
  val dummyId = Int.MinValue
  /*val dummyNamedType = NamedType(0, "DummyType")
  val dummyArrowType = Arrow(dummyNamedType, dummyNamedType)*/
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"
  val scopeSeparator : String = "."


  type AbstractionMap = SetValueMap[NodeId, (NodeId, AbstractionPolicy)]
  val AbstractionMap = SetValueMap

  type Nodes2VnodeMap = Map[Seq[NodeId], NodeId]
  val Nodes2VNodeMap = Map

  type ConcreteNodeIndex = Map[NodeId, ConcreteNode]
  val ConcreteNodeIndex = Map
  type VirtualNodeIndex = Map[NodeId, VirtualNode]
  val VirtualNodeINdex = Map
  
  implicit def idToNode(implicit graph : DependencyGraph, id : NodeId) : DGNode =
               graph.getNode(id)

  type Mutability = Boolean

  def areEquivalent[Kind <: NodeKind, T](initialRecord : Seq[Transformation],
                      graph1 : DependencyGraph,
                      graph2 : DependencyGraph,
                      logger : PuckLogger = PuckNoopLogger) : Boolean = {
    val engine = new RecordingComparator(initialRecord, graph1, graph2, logger)
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
  private [this] val edges : EdgeMap,
  /*private [this]*/ val abstractionsMap : AbstractionMap,
  val constraints : ConstraintsMaps,
  val recording : Recording) {


  type GraphT = DependencyGraph

  def newGraph(nLogger : PuckLogger = logger,
               nIdSeed : Int = idSeed,
               nNodesSet : ConcreteNodeIndex = nodesIndex,
               nRemovedNodes : ConcreteNodeIndex = removedNodes,
               nVNodesIndex : VirtualNodeIndex = vNodesIndex,
               nVRemovedNodes : VirtualNodeIndex = vRemovedNodes,
               nNodes2vNodes : Nodes2VnodeMap = nodes2vNodes,
               nEdges : EdgeMap = edges,
               nAbstractionsMap : AbstractionMap = abstractionsMap,
               nConstraints : ConstraintsMaps = constraints,
               nRecording : Recording = recording) : DependencyGraph =
    new DependencyGraph(nLogger, nodeKindKnowledge, nIdSeed,
                        nNodesSet, nRemovedNodes,
                        nVNodesIndex, nVRemovedNodes,
                        nNodes2vNodes,
                        nEdges,
                        nAbstractionsMap, nConstraints, nRecording)

  def withLogger(l : PuckLogger) = newGraph(nLogger = l)
  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.InGraph, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.InGraph, lvl)


  def comment(msg : String) = newGraph(nRecording = recording.comment(msg))
  def mileStone = newGraph(nRecording = recording.mileStone)

  val rootId : NodeId = 0
  def root : ConcreteNode = getConcreteNode(rootId)
  def isRoot(id : NodeId) = id == rootId

  override def toString = edges.toString

  private [graph] def addConcreteNode(n : ConcreteNode) : DependencyGraph =
     newGraph(nNodesSet = nodesIndex + (n.id -> n),
       nRemovedNodes = removedNodes - n.id,
       nRecording = recording.addConcreteNode(n))

  def addConcreteNode
  ( localName : String,
    kind : NodeKind,
    th : Option[Type],
    mutable : Mutability = true
    ) : (ConcreteNode, GraphT) = {
    val nid = idSeed + 1
    val n = ConcreteNode(nid, localName, kind, th, mutable)
    (n, newGraph(nIdSeed = nid).addConcreteNode(n))
  }

  private def addVirtualNode
  ( n : VirtualNode ) : DependencyGraph =
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

  def numRemovedNodes : Int = removedNodes .size + vRemovedNodes.size

  def isaEdges : List[DGEdge] = edges.superTypes.content.foldLeft(List[DGEdge]()){
    case (acc, (sub, sups)) =>
      sups.toList.map(sup => DGEdge.IsaK(sub, sup)) ::: acc
  }

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

  def setName(id : NodeId, newName : String) : GraphT = {
    val (n, s) = getConcreteNodeWithStatus(id)
    val g1 = setNode(n.copy(name = newName), s)
    g1.newGraph(nRecording = recording.changeNodeName(id, n.name, newName))
  }
  
  def setType(id : NodeId, st : Option[Type]) : GraphT = {
    val (n, s) = getConcreteNodeWithStatus(id)
    setNode(n.copy(styp = st), s)
  }

  def setMutability(id : NodeId, mutable : Boolean) =  {
    val (n, s) = getConcreteNodeWithStatus(id)
    setNode(n.copy(mutable = mutable), s)
  }


  def exists(e : DGEdge) : Boolean =
    edges.exists(e)

  def addEdge(e : DGEdge, register : Boolean = true): GraphT =
    newGraph( nEdges = edges.add(e),
              nRecording =
                  if(register) recording.addEdge(e)
                  else recording)

  def removeEdge(e : DGEdge, register : Boolean = true): GraphT =
    newGraph( nEdges = edges.remove(e),
              nRecording =
                  if(register) recording.removeEdge(e)
                  else recording)

 def addContains(containerId: NodeId, contentId :NodeId, register : Boolean = true): GraphT =
    addEdge(Contains(containerId, contentId), register)

 def removeContains(containerId: NodeId, contentId :NodeId, register : Boolean = true): GraphT =
    removeEdge(Contains(containerId, contentId), register)

 def addUses(userId: NodeId, useeId: NodeId, register : Boolean = true): GraphT =
    addEdge(Uses(userId, useeId))

 def removeUses(userId: NodeId, useeId: NodeId, register : Boolean = true): GraphT =
    removeEdge(Uses(userId, useeId))

 def addIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : GraphT=
    addEdge(Isa(subTypeId, superTypeId))

 def removeIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : GraphT=
    removeEdge(Isa(subTypeId, superTypeId))


 def addUsesDependency(typeUse : DGUses,
                        typeMemberUse : DGUses) : GraphT =
   newGraph(nEdges = edges.addUsesDependency(typeUse, typeMemberUse),
     nRecording = recording.addTypeDependency(typeUse, typeMemberUse))



  def removeUsesDependency(typeUse : DGUses,
                           typeMemberUse : DGUses) : GraphT =
      newGraph(nEdges = edges.removeUsesDependency(typeUse, typeMemberUse),
        nRecording = recording.removeTypeDependency(typeUse, typeMemberUse))



  def addAbstraction(id : NodeId, abs : (NodeId, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap + (id, abs),
             nRecording = recording.addAbstraction(id, abs._1, abs._2))

  def removeAbstraction(id : NodeId, abs : (NodeId, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap - (id, abs),
             nRecording = recording.removeAbstraction(id, abs._1, abs._2))

  def changeTarget(edge : DGEdge, newTarget : NodeId) : GraphT = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge : DGEdge = edge.kind(edge.source, newTarget)
    val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = newEdge.existsIn(this))
    newEdge.createIn(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeSource(edge : DGEdge, newSource : NodeId) : GraphT = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge: DGEdge = edge.kind(newSource, edge.target)
    val newRecording = recording.changeEdgeSource(edge, newSource, withMerge = newEdge.existsIn(this))
    newEdge.createIn(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeType(id : NodeId, styp : Option[Type], oldUsee: NodeId, newUsee : NodeId) : GraphT =
    styp match {
      case None => this
      case Some(t) => val newTyp= Some(t.changeNamedType(oldUsee, newUsee))
        setType(id, newTyp).
          newGraph(nRecording = recording.addTypeChange(id, styp, oldUsee, newUsee))
    }

  def changeContravariantType(id : NodeId, styp : Option[Type], oldUsee: NodeId, newUsee : NodeId) : GraphT =
  styp match {
    case None => this
    case Some(t) => val newTyp= Some(t.changeNamedTypeContravariant(oldUsee, newUsee))
      setType(id, newTyp).
        newGraph(nRecording = recording.addTypeChange(id, styp, oldUsee, newUsee))
  }

  /*
   * Read-only queries
   */

  def nodeKinds : Seq[NodeKind] = nodeKindKnowledge.nodeKinds

  def container(contentId : NodeId) : Option[NodeId] = edges.containers.get(contentId)

  def content(containerId: NodeId) : Set[NodeId] = edges.contents.getFlat(containerId)

  def contains(containerId : NodeId, contentId : NodeId) : Boolean =
    edges.contains(containerId, contentId)

  def containsSeq : Seq[(NodeId, NodeId)] = edges.contents.flatSeq

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
    val path = containerPath(id).map{n => showDG[DGNode](this)(nodeNameCord).shows(getNode(n))}

    (if (path.head == DependencyGraph.rootName)
      path.tail
    else
      DependencyGraph.unrootedStringId +: path ).mkString(DependencyGraph.scopeSeparator)
  }


  def directSuperTypes(sub: NodeId) : Iterable[NodeId] = edges.superTypes getFlat sub
  def directSubTypes(sup: NodeId) : Iterable[NodeId] = edges.subTypes getFlat sup

  def subTypes(sup : NodeId) : Iterable[NodeId]= {
    val dst = directSubTypes(sup).toSeq
    dst.foldLeft(dst) { case (acc, id) => acc ++ subTypes(id) }
  }

  def isSuperTypeOf(superCandidate: NodeId, subCandidate : NodeId) : Boolean = {
    directSuperTypes(subCandidate).exists(_ == superCandidate) ||
      directSuperTypes(subCandidate).exists(isSuperTypeOf(superCandidate, _))
  }

  def isa(subId : NodeId, superId: NodeId): Boolean =
    edges.isa(subId, superId)
  
  def isaSeq  : Seq[(NodeId, NodeId)] = edges.superTypes.flatSeq
  
  def uses(userId: NodeId, usedId: NodeId) : Boolean =
    edges.uses(userId, usedId)

  def usesSeq : Seq[(NodeId, NodeId)] =
    edges.used.flatSeq
  
  def usedBy(userId : NodeId) : Set[NodeId] =
    edges.used getFlat userId
  
  def usersOf(usedId: NodeId) : Set[NodeId] =
    edges.users getFlat usedId

  // ugly name
  def usesOfUsersOf(usedIds: Seq[NodeId]) : Seq[Uses] =
    usedIds flatMap {
      tmid =>
        this.usersOf(tmid) map (user => Uses(user,tmid))
    }

  def usesOfUsersOf(usedId: NodeId) : Seq[Uses] = usesOfUsersOf(Seq(usedId))


  def typeUsesOf(typeMemberUse : DGUses) : Set[DGUses] =
    edges typeUsesOf typeMemberUse

  def typeMemberUsesOf(typeUse : DGUses) : Set[DGUses] =
    edges typeMemberUsesOf typeUse

  def typeUsesOf(tmUser : NodeId, tmUsed : NodeId) : Set[DGUses] =
    edges typeUsesOf (tmUser, tmUsed)

  def typeMemberUsesOf(typeUser : NodeId, typeUsed : NodeId) : Set[DGUses] =
    edges typeMemberUsesOf (typeUser, typeUsed)

  def typeMemberUses2typeUses : Seq[(DGUses, Set[DGUses])] =
    edges.typeMemberUses2typeUsesMap.toSeq

  def typeUses2typeMemberUses : Seq[(DGUses, Set[DGUses])] =
    edges.typeUses2typeMemberUsesMap.toSeq

  def bind(typeUse : DGUses,
           typeMemberUse : DGUses) : Boolean =
    typeMemberUsesOf( typeUse ) contains typeMemberUse

  def abstractions(id : NodeId) : Set[(NodeId, AbstractionPolicy)] =
    abstractionsMap getFlat id

  def isAbstraction(implId : NodeId, absId : NodeId, pol : AbstractionPolicy) : Boolean =
    abstractionsMap bind (implId, (absId, pol))

  def isAbstraction(implId : NodeId, absId : NodeId) : Option[AbstractionPolicy] = {
    if(abstractionsMap bind (implId, (absId, SupertypeAbstraction)))
      Some(SupertypeAbstraction)
    else if(abstractionsMap bind (implId, (absId, DelegationAbstraction)))
      Some(DelegationAbstraction)
    else
      None
  }


  def violations() : Seq[DGEdge] =
    concreteNodesId.flatMap { n =>
      val wu = constraints.wrongUsers(this, n).map(DGEdge.UsesK(_,n))
      if(constraints.isWronglyContained(this, n))
         DGEdge.ContainsK(container(n).get, n) +: wu
      else wu
    }.toSeq

  def isViolation(e : DGEdge) : Boolean = {
    e.kind match {
      case DGEdge.ContainsK =>
        constraints.isWronglyContained(this, e.target)
      case DGEdge.UsesK | DGEdge.IsaK =>
        constraints.violation(this, e.user, e.used)

      case DGEdge.ParameterizedUsesK => false
    }
  }

  def wrongUsers(id : NodeId) : Seq[NodeId] = constraints.wrongUsers(this, id)
  def isWronglyContained(id : NodeId) = constraints.isWronglyContained(this, id)
  def interloperOf(id1 : NodeId, id2 :NodeId) = constraints.violation(this, id1, id2)

  def printConstraints[V](logger : Logger[V], v : V) : Unit =
    constraints.printConstraints(this, logger, v)

 def coupling = nodeKindKnowledge.coupling(this)

  def subTree(root : NodeId, includeRoot : Boolean = true) : Seq[NodeId] = {

    def aux(acc : Seq[NodeId])( roots : Seq[NodeId]): Seq[NodeId] = roots match {
//    type Roots = Seq[NodeId]
//    @tailrec
//    def aux(acc : Seq[NodeId]) : Roots => Seq[NodeId] = {
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