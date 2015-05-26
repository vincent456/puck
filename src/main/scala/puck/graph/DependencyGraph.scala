package puck.graph

import puck.graph.DependencyGraph._
import puck.graph.constraints._
import puck.graph.transformations.{Transformation, RecordingComparator}
import puck.util.{Logger, PuckNoopLogger, PuckLogger, PuckLog}

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


  
  implicit def idToNode(implicit graph : DependencyGraph, id : NodeId) : DGNode =
               graph.getNode(id)



  def areEquivalent[Kind <: NodeKind, T](initialRecord : Seq[Transformation],
                      graph1 : DependencyGraph,
                      graph2 : DependencyGraph,
                      logger : PuckLogger = PuckNoopLogger) : Boolean =
     new RecordingComparator(initialRecord, graph1, graph2, logger).compare()


}


class DependencyGraph
( val logger : PuckLogger = PuckNoopLogger,
  val nodeKindKnowledge: NodeKindKnowledge,
  private [this] val nodesIndex : NodeIndex,
  private [this] val edges : EdgeMap,
  /*private [this]*/ val abstractionsMap : AbstractionMap,
  val constraints : ConstraintsMaps,
  val recording : Recording) {


  def newGraph(nLogger : PuckLogger = logger,
               nodes : NodeIndex = nodesIndex,
               edges : EdgeMap = edges,
               abstractionsMap : AbstractionMap = abstractionsMap,
               constraints : ConstraintsMaps = constraints,
               recording : Recording = recording) : DependencyGraph =
    new DependencyGraph(nLogger, nodeKindKnowledge,
                        nodes, edges,
                        abstractionsMap, constraints, recording)

  def withLogger(l : PuckLogger) = newGraph(nLogger = l)
  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.InGraph, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.InGraph, lvl)


  def comment(msg : String) = newGraph(recording = recording.comment(msg))
  def mileStone = newGraph(recording = recording.mileStone)

  val rootId : NodeId = 0
  def root : ConcreteNode = getConcreteNode(rootId)
  def isRoot(id : NodeId) = id == rootId

  override def toString = edges.toString

  private [graph] def addConcreteNode(n : ConcreteNode) : DependencyGraph =
     newGraph(nodes = nodesIndex.addConcreteNode(n),
              recording = recording.addConcreteNode(n))

  def addConcreteNode
  ( localName : String,
    kind : NodeKind,
    th : Option[Type],
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) = {
    val(n, nIndex) = nodesIndex.addConcreteNode(localName, kind, th, mutable)
    (n, newGraph(nodes = nIndex,
      recording = recording.addConcreteNode(n)))
  }

  private def addVirtualNode
  ( n : VirtualNode ) : DependencyGraph =
    newGraph(nodes = nodesIndex.addVirtualNode(n),
      recording = recording.addVirtualNode(n))


  def addVirtualNode(ns : Seq[NodeId], k : NodeKind) : (VirtualNode, DependencyGraph) = {
    val (vn, nIndex) = nodesIndex.addVirtualNode(ns, k)
    (vn, newGraph(nodes = nIndex))
  }

  def nodes : Iterable[DGNode] = nodesIndex.nodes
  def concreteNodes : Iterable[ConcreteNode] = nodesIndex.concreteNodes
  def virtualNodes : Iterable[VirtualNode] = nodesIndex.virtualNodes

  def nodesId : Iterable[NodeId] = nodesIndex.nodesId
  def concreteNodesId : Iterable[NodeId] = nodesIndex.concreteNodesId

  def numNodes : Int = nodesIndex.numNodes
  def numRemovedNodes : Int = nodesIndex.numRemovedNodes

  def isaEdges : List[DGEdge] = edges.superTypes.content.foldLeft(List[DGEdge]()){
    case (acc, (sub, sups)) =>
      sups.toList.map(sup => DGEdge.IsaK(sub, sup)) ::: acc
  }




  def getNode(id : NodeId): DGNode = nodesIndex.getNode(id)

  def getConcreteNode(id : NodeId): ConcreteNode =
    nodesIndex.getConcreteNode(id)


  def removeConcreteNode(id : NodeId) = {
      val (n, index) = nodesIndex.removeConcreteNode(id)
      newGraph(nodes = index, recording = recording removeConcreteNode n)
  }

  def removeVirtualNode(id : NodeId) = {
    val (n, index) = nodesIndex.removeVirtualNode(id)
    newGraph(nodes = index, recording = recording removeVirtualNode n)
  }


  def setName(id : NodeId, newName : String) : DependencyGraph = {
    val (oldName, index) = nodesIndex.setName(id, newName)
    newGraph( nodes = index,
      recording = recording.changeNodeName(id, oldName, newName))
  }
  
  def setType(id : NodeId, st : Option[Type]) : DependencyGraph =
    newGraph(nodes = nodesIndex.setType(id, st))

  def setMutability(id : NodeId, mutable : Boolean) =
    newGraph(nodes = nodesIndex.setMutability(id, mutable))


  def exists(e : DGEdge) : Boolean =
    edges.exists(e)

  def addEdge(e : DGEdge, register : Boolean = true): DependencyGraph =
    newGraph( edges = edges.add(e),
              recording =
                  if(register) recording.addEdge(e)
                  else recording)

  def removeEdge(e : DGEdge, register : Boolean = true): DependencyGraph =
    newGraph( edges = edges.remove(e),
              recording =
                  if(register) recording.removeEdge(e)
                  else recording)

 def addContains(containerId: NodeId, contentId :NodeId, register : Boolean = true): DependencyGraph =
    addEdge(Contains(containerId, contentId), register)

 def removeContains(containerId: NodeId, contentId :NodeId, register : Boolean = true): DependencyGraph =
    removeEdge(Contains(containerId, contentId), register)

 def addUses(userId: NodeId, useeId: NodeId, register : Boolean = true): DependencyGraph =
    addEdge(Uses(userId, useeId))

 def removeUses(userId: NodeId, useeId: NodeId, register : Boolean = true): DependencyGraph =
    removeEdge(Uses(userId, useeId))

 def addIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : DependencyGraph=
    addEdge(Isa(subTypeId, superTypeId))

 def removeIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : DependencyGraph=
    removeEdge(Isa(subTypeId, superTypeId))


 def addUsesDependency(typeUse : DGUses,
                        typeMemberUse : DGUses) : DependencyGraph =
   newGraph(edges = edges.addUsesDependency(typeUse, typeMemberUse),
     recording = recording.addTypeDependency(typeUse, typeMemberUse))



  def removeUsesDependency(typeUse : DGUses,
                           typeMemberUse : DGUses) : DependencyGraph =
      newGraph(edges = edges.removeUsesDependency(typeUse, typeMemberUse),
        recording = recording.removeTypeDependency(typeUse, typeMemberUse))



  def addAbstraction(id : NodeId, abs : (NodeId, AbstractionPolicy)) : DependencyGraph =
    newGraph(abstractionsMap = abstractionsMap + (id, abs),
             recording = recording.addAbstraction(id, abs._1, abs._2))

  def removeAbstraction(id : NodeId, abs : (NodeId, AbstractionPolicy)) : DependencyGraph =
    newGraph(abstractionsMap = abstractionsMap - (id, abs),
             recording = recording.removeAbstraction(id, abs._1, abs._2))

  def changeTarget(edge : DGEdge, newTarget : NodeId) : DependencyGraph = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge : DGEdge = edge.kind(edge.source, newTarget)
    val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = newEdge.existsIn(this))
    newEdge.createIn(g1, register = false).newGraph(recording = newRecording)
  }

  def changeSource(edge : DGEdge, newSource : NodeId) : DependencyGraph = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge: DGEdge = edge.kind(newSource, edge.target)
    val newRecording = recording.changeEdgeSource(edge, newSource, withMerge = newEdge.existsIn(this))
    newEdge.createIn(g1, register = false).newGraph(recording = newRecording)
  }

  def changeType(id : NodeId, styp : Option[Type], oldUsee: NodeId, newUsee : NodeId) : DependencyGraph =
    styp match {
      case None => this
      case Some(t) => val newTyp= Some(t.changeNamedType(oldUsee, newUsee))
        setType(id, newTyp).
          newGraph(recording = recording.addTypeChange(id, styp, oldUsee, newUsee))
    }

  def changeContravariantType(id : NodeId, styp : Option[Type], oldUsee: NodeId, newUsee : NodeId) : DependencyGraph =
  styp match {
    case None => this
    case Some(t) => val newTyp= Some(t.changeNamedTypeContravariant(oldUsee, newUsee))
      setType(id, newTyp).
        newGraph(recording = recording.addTypeChange(id, styp, oldUsee, newUsee))
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

  def canBe(n : DGNode, cn : ConcreteNode) : Boolean =
    nodeKindKnowledge.canBe(this)(n,cn)

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

  def isa(subId : NodeId, superId: NodeId): Boolean =
    edges.isa(subId, superId)

  def isa_*(subId : NodeId, superId: NodeId): Boolean =
    edges.isa_*(subId, superId)
  //  {
  //    directSuperTypes(subCandidate).exists(_ == superCandidate) ||
  //      directSuperTypes(subCandidate).exists(isSuperTypeOf(superCandidate, _))
  //  }

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