package puck.graph

import java.util.NoSuchElementException

import puck.graph.DependencyGraph.AbstractionMap
import puck.graph.constraints.{ConstraintsMaps, AbstractionPolicy}
import puck.util.{Logger, PuckNoopLogger, PuckLogger, PuckLog}

import puck.graph.transformations.{RecordingComparator, Transformation, RecordingOps}

object DependencyGraph {

  val rootId : NodeId = 0
  val dummyId = Int.MinValue
  /*val dummyNamedType = NamedType(0, "DummyType")
  val dummyArrowType = Arrow(dummyNamedType, dummyNamedType)*/
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"
  val scopeSeparator : String = "."

  val anonymousName : String = "Anonymous"

  type AbstractionMap = SetValueMap.T[NodeId, Abstraction]
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
( //val logger : PuckLogger = PuckNoopLogger,
  val nodeKindKnowledge: NodeKindKnowledge,
  /*private [this]*/ val nodesIndex : NodeIndex,
  /*private [this]*/ val edges : EdgeMap,
  /*private [this]*/ val abstractionsMap : AbstractionMap,
  val constraints : ConstraintsMaps,
  val recording : Recording) {

  def rootKind = nodeKindKnowledge.rootKind

  def newGraph(//nLogger : PuckLogger = logger,
               nodesIndex : NodeIndex = nodesIndex,
               edges : EdgeMap = edges,
               abstractionsMap : AbstractionMap = abstractionsMap,
               constraints : ConstraintsMaps = constraints,
               recording : Recording = recording) : DependencyGraph =
    new DependencyGraph(//nLogger,
                        nodeKindKnowledge,
                        nodesIndex, edges,
                        abstractionsMap, constraints, recording)

  //def withLogger(l : PuckLogger) = newGraph(nLogger = l)
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
     newGraph(nodesIndex = nodesIndex.addConcreteNode(n),
              recording = recording.addConcreteNode(n))

  def addConcreteNode
  ( localName : String,
    kind : NodeKind,
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) = {
    val(n, nIndex) = nodesIndex.addConcreteNode(localName, kind, mutable)
    (n, newGraph(nodesIndex = nIndex,
      recording = recording.addConcreteNode(n)))
  }

  private [graph] def addVirtualNode
  ( n : VirtualNode ) : DependencyGraph =
    newGraph(nodesIndex = nodesIndex.addVirtualNode(n),
      recording = recording.addVirtualNode(n))


  def addVirtualNode(ns : Seq[NodeId], k : NodeKind) : (VirtualNode, DependencyGraph) = {
    val (vn, nIndex) = nodesIndex.addVirtualNode(ns, k)
    (vn, newGraph(nodesIndex = nIndex,
      recording = recording.addVirtualNode(vn)))
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
      sups.toList.map(sup => Isa(sub, sup)) ::: acc
  }

  def getNode(id : NodeId): DGNode = nodesIndex.getNode(id)

  def getConcreteNode(id : NodeId): ConcreteNode =
    nodesIndex.getConcreteNode(id)


  def removeConcreteNode(n : ConcreteNode) : DependencyGraph =
      newGraph(nodesIndex = nodesIndex removeConcreteNode n,
        recording = recording removeConcreteNode n)


  def removeVirtualNode(n : VirtualNode) : DependencyGraph =
    newGraph(nodesIndex = nodesIndex removeVirtualNode n,
      recording = recording removeVirtualNode n)


  def removeNode(id: NodeId) : (DGNode, DependencyGraph) = {
    getNode(id) match {
      case vn : VirtualNode => (vn, removeVirtualNode(vn))
      case cn : ConcreteNode => (cn, removeConcreteNode(cn))
    }
  }

  def setName(id : NodeId, newName : String) : DependencyGraph = {
    val (oldName, index) = nodesIndex.setName(id, newName)
    newGraph( nodesIndex = index,
      recording = recording.changeNodeName(id, oldName, newName))
  }

  def setRole(id : NodeId, srole : Option[Role]) : DependencyGraph = {
      newGraph(nodesIndex = nodesIndex.setRole(id, srole),
        recording = recording.addRoleChange(id, getRole(id), srole))
  }
  def getRole(id: NodeId) : Option[Role] = nodesIndex.getRole(id)

  def styp(id : NodeId) : Option[Type] = edges.types get id

  def structuredType(id : NodeId) : Option[Type] = {
    //assert node is a typed value
    nodeKindKnowledge.structuredType(this, id, parameters(id))
  }

  def typedNode(id : NodeId ) : TypedNode = {
    val cn = getConcreteNode(id)
        styp(id) match {
          case None => error(s"$cn has no type")
          case Some(t) => (cn, t)
      }
  }


  def setType(id : NodeId, t : Option[Type]) : DependencyGraph = {
    //println(s"setting type of ${getNode(id)}  to $t")

    newGraph(edges = edges.setType(id, t),
      recording = recording.addTypeChange(id, styp(id), t))
  }


  def changeType
  ( id : NodeId,
    oldUsed: NodeId,
    newUsed : NodeId) : DependencyGraph =
    styp(id) match {
      case None => this
      case Some(t) => setType(id, Some(t.changeNamedType(oldUsed, newUsed)))
    }


  def setMutability(id : NodeId, mutable : Boolean) =
    newGraph(nodesIndex = nodesIndex.setMutability(id, mutable))


  def exists(e : DGEdge) : Boolean = edges.exists(e)

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

 def addUses(userId: NodeId, useeId: NodeId,
             sAccessKind : Option[UsesAccessKind] = None,
             register : Boolean = true): DependencyGraph =
    addEdge(Uses(userId, useeId, sAccessKind))


 def addIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : DependencyGraph=
    addEdge(Isa(subTypeId, superTypeId))

 def removeIsa(subTypeId: NodeId, superTypeId: NodeId, register : Boolean = true) : DependencyGraph=
    removeEdge(Isa(subTypeId, superTypeId))

 def addDef(decl : NodeId, _def : NodeId) : DependencyGraph =
    addEdge(ContainsDef(decl, _def))

 def addParam(decl : NodeId, param : NodeId) : DependencyGraph =
    addEdge(ContainsParam(decl, param))

 def addUsesDependency
 ( typeUse : NodeIdP,
   typeMemberUse : NodeIdP) : DependencyGraph =
      newGraph(edges = edges.addUsesDependency(typeUse, typeMemberUse),
          recording = recording.addTypeDependency(typeUse, typeMemberUse))

 def removeUsesDependency
 ( typeUse : NodeIdP,
   typeMemberUse : NodeIdP) : DependencyGraph =
      newGraph(edges = edges.removeUsesDependency(typeUse, typeMemberUse),
        recording = recording.removeTypeDependency(typeUse, typeMemberUse))

 def changeTypeUseOfTypeMemberUse
 ( oldTypeUse : NodeIdP,
   newTypeUse : NodeIdP,
   tmUse : NodeIdP) : DependencyGraph = {
   newGraph(edges =
      edges.removeUsesDependency(oldTypeUse, tmUse).
            addUsesDependency(newTypeUse, tmUse),
     recording = recording.changeTypeUseOfTypeMemberUse(oldTypeUse, newTypeUse, tmUse))
 }

//  def changeTypeUseOfTypeMemberUseList
//  ( oldTypeUse : NodeIdP,
//    newTypeUse : NodeIdP,
//    tmUses : List[DGUses]
//    ) : DependencyGraph = {
//    tmUses
//    newGraph(edges =
//      edges.removeUsesDependency(oldTypeUse, tmUse).
//        addUsesDependency(newTypeUse, tmUse),
//      recording = recording.changeTypeUseOfTypeMemberUse(oldTypeUse, newTypeUse, tmUse))
//  }

  def changeTypeMemberUseOfTypeUse
  ( oldTmUse : NodeIdP,
    newTmUse : NodeIdP,
    typeUse : NodeIdP) : DependencyGraph = {
    newGraph(edges =
      edges.removeUsesDependency(typeUse, oldTmUse).
        addUsesDependency(typeUse, newTmUse),
      recording = recording.changeTypeMemberUseOfTypeUse(oldTmUse, newTmUse, typeUse))
  }

  def addAbstraction(id : NodeId, abs : Abstraction) : DependencyGraph =
    newGraph(abstractionsMap = abstractionsMap + (id, abs),
             recording = recording.addAbstraction(id, abs))

  def removeAbstraction(id : NodeId, abs : Abstraction) : DependencyGraph =
    newGraph(abstractionsMap = abstractionsMap - (id, abs),
             recording = recording.removeAbstraction(id, abs))


  private def isChangeType(edge : DGEdge, newTarget : NodeId) : Boolean = {
    edge.kind == Uses && (getNode(edge.user).kind.kindType match {
      case InstanceTypeDecl
        | StaticValueDecl
        | Parameter =>
        val oldUsedKind = getNode(edge.used).kind.kindType
        val newUsedKind = getNode(newTarget).kind.kindType

        oldUsedKind == TypeDecl && newUsedKind == TypeDecl
      case _ => false
    })

  }
  def changeTarget(edge : DGEdge, newTarget : NodeId) : DependencyGraph =
    if(isChangeType(edge, newTarget))
       changeType(edge.user, edge.used, newTarget)
    else {
      val g1 = edge.deleteIn(this, register = false)
      val newEdge : DGEdge = edge.copy(target = newTarget)
      val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = newEdge.existsIn(this))
      newEdge.createIn(g1, register = false).newGraph(recording = newRecording)
    }

  def changeSource(edge : DGEdge, newSource : NodeId) : DependencyGraph = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge: DGEdge = edge.copy(source = newSource)
    val newRecording = recording.changeEdgeSource(edge, newSource, withMerge = newEdge.existsIn(this))
    newEdge.createIn(g1, register = false).newGraph(recording = newRecording)
  }

  /*
   * Read-only queries
   */

  def nodeKinds : Seq[NodeKind] = nodeKindKnowledge.nodeKinds

  def container(contentId : NodeId) : Option[NodeId] =
    edges.containers.get(contentId)

  def container_!(contentId : NodeId) : NodeId =
    container(contentId).get

  def containerOfKindType(kt: KindType, nid : NodeId) : NodeId =
    getNode(nid).kind.kindType match {
      case `kt` => nid
      case _ => containerOfKindType(kt, container_!(nid))
    }

  def hostNameSpace(nid : NodeId) : NodeId =
    containerOfKindType(NameSpace, nid)

  def hostTypeDecl(nid : NodeId) : NodeId =
    containerOfKindType(TypeDecl, nid)

  def content(containerId: NodeId) : Set[NodeId] =
    edges.contents.getFlat(containerId) /*++
      (definitionOf(containerId) map (Set(_))).getOrElse(Set()) ++
      edges.parameters.getFlat(containerId)*/



  //special case alias for readibility
  def declarationOf(defId : NodeId) : NodeId =
    container_!(defId)

  //special cases of content
  def definitionOf(declId : NodeId) : Option[NodeId] =
  //assert kindType == InstanceValueDecl || StaticValueDecl || TypeConstructor
    edges.definition get declId

  def definitionOf_!(declId : NodeId) : NodeId =
    edges definition declId



  def parameters(declId : NodeId)  : List[NodeId] = edges.parameters.getFlat(declId)


  def contains(containerId : NodeId, contentId : NodeId) : Boolean =
    edges.contains(containerId, contentId)

  def containsList : List[(NodeId, NodeId)] = edges.contents.flatList

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
    val path = containerPath(id).map{n => (this, getNode(n)).shows(nodeNameCord)}

    (if (path.head == DependencyGraph.rootName)
      path.tail
    else
      DependencyGraph.unrootedStringId +: path ).mkString(DependencyGraph.scopeSeparator)
  }


  def directSuperTypes(sub: NodeId) : Set[NodeId] = edges.superTypes getFlat sub
  def directSubTypes(sup: NodeId) : Set[NodeId] = edges.subTypes getFlat sup

  def subTypes(sup : NodeId) : Set[NodeId]= {
    val dst = directSubTypes(sup)
    dst.foldLeft(dst) { case (acc, id) => acc ++ subTypes(id) }
  }

  def isa(subId : NodeId, superId: NodeId): Boolean =
    edges.isa(subId, superId)

  def isa_*(subId : NodeId, superId: NodeId): Boolean =
    edges.isa_*(subId, superId)

  def isaList  : List[(NodeId, NodeId)] = edges.superTypes.flatList

  def usesAccessKind(userId: NodeId, usedId: NodeId) : Option[UsesAccessKind] =
    edges.accessKindMap.get((userId, usedId))

  def getUsesEdge(userId: NodeId, usedId: NodeId) : Option[DGUses] =
    edges.getUses(userId, usedId)

  def getUsesEdge_!(userId: NodeId, usedId: NodeId) : DGUses =
    getUsesEdge(userId, usedId).get

  def uses(userId: NodeId, usedId: NodeId) : Boolean =
    edges.uses(userId, usedId)

  def usesList : List[(NodeId, NodeId)] =
    edges.usedMap.flatList
  
  def usedBy(userId : NodeId) : Set[NodeId] =
    edges.usedMap getFlat userId
  
  def usersOf(usedId: NodeId) : Set[NodeId] =
    edges.userMap getFlat usedId

  // ugly name
  def usesFromUsedList(usedIds: List[NodeId]) : List[DGUses] =
    usedIds flatMap {
      tmid => this.usersOf(tmid) map (user => this.getUsesEdge(user,tmid).get)
    }

  def usesFromUsedList(usedId: NodeId) : List[DGUses] = usesFromUsedList(List(usedId))

  def typeUsesOf(typeMemberUse : DGUses) : Set[DGUses] =
    edges typeUsesOf typeMemberUse

  def typeMemberUsesOf(typeUse : DGUses) : Set[DGUses] =
    edges typeMemberUsesOf typeUse

  def typeUsesOf(tmUser : NodeId, tmUsed : NodeId) : Set[DGUses] =
    edges typeUsesOf (tmUser, tmUsed)

  def typeMemberUsesOf(typeUser : NodeId, typeUsed : NodeId) : Set[DGUses] =
    edges typeMemberUsesOf (typeUser, typeUsed)

  def typeMemberUses2typeUses : Seq[(NodeIdP, Set[NodeIdP])] =
    edges.typeMemberUses2typeUsesMap.toSeq

  def typeUses2typeMemberUses : Seq[(NodeIdP, Set[NodeIdP])] =
    edges.typeUses2typeMemberUsesMap.toSeq

  def bind(typeUse : DGUses,
           typeMemberUse : DGUses) : Boolean =
    typeMemberUsesOf( typeUse ) contains typeMemberUse

  def abstractions(id : NodeId) : Set[Abstraction] =
    abstractionsMap getFlat id

  def isAbstraction(implId : NodeId, absId : NodeId, pol : AbstractionPolicy) : Boolean =
    isAbstraction(implId, absId).exists{_.policy == pol}

  def isAbstraction(implId : NodeId, absId : NodeId) : Option[Abstraction] =
    abstractionsMap get implId flatMap {
      absSet =>
        absSet find {
          case AccessAbstraction(id, _) => id == absId
          case ReadWriteAbstraction(rId, wId) =>
            rId.contains(absId) || wId.contains(absId)
        }
    }



  def violations() : Seq[DGEdge] =
    concreteNodesId.flatMap { n =>
      val wu = constraints.wrongUsers(this, n).map(Uses(_,n))
      if(constraints.isWronglyContained(this, n))
         Contains(container(n).get, n) +: wu
      else wu
    }.toSeq

  def isViolation(e : DGEdge) : Boolean = {
    e.kind match {
      case Contains | ContainsDef | ContainsParam =>
        constraints.isWronglyContained(this, e.target)
      case Uses | Isa =>
        constraints.isViolation(this, e.user, e.used)

      case ParameterizedUses => false
    }
  }

  def wrongUsers(id : NodeId) : List[NodeId] = constraints.wrongUsers(this, id)
  def interloperOf(id1 : NodeId, id2 :NodeId) = constraints.isViolation(this, id1, id2)
  def isWronglyUsed(id : NodeId) = constraints.wrongUsers(this, id).nonEmpty
  def isWronglyContained(id : NodeId) : Boolean = constraints.isWronglyContained(this, id)

  def printConstraints[V](logger : Logger[V], v : V) : Unit =
    constraints.printConstraints(this, logger, v)


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
    getNode(nid).kind.kindType

  def getDefaultConstructorOfType(typeId : NodeId) : Option[NodeId] =
    nodeKindKnowledge.getConstructorOfType(this, typeId)


}