package puck.graph.immutable

import puck.graph.AGError
import puck.graph.constraints.AbstractionPolicy
import puck.util.{PuckLog, PuckNoopLogger, PuckLogger}
import puck.graph.immutable.transformations.Recording


object AccessGraph {

  val rootId = 0
  val dummyId = 0
  val dummyNamedType = NamedType(0, "DummyType")
  val dummyArrowType = Arrow(dummyNamedType, dummyNamedType)
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"
  val scopeSeparator : String = "."

  //phantom type of NodeId used to ease Mutable/Immutable transition
  //in mutable NodeId[K] =:= AGNode[K]
  type NodeId[Kind <: NodeKind[Kind]] = Int
  type EdgeMap[Kind <: NodeKind[Kind]] = SetValueMap[NodeId[Kind], NodeId[Kind]]
  val EdgeMap = SetValueMap
  type UseDependencyMap[Kind <: NodeKind[Kind]] =
       SetValueMap[(NodeId[Kind],NodeId[Kind]), (NodeId[Kind],NodeId[Kind])]
  val UseDependencyMap = SetValueMap
  type AbstractionMap[Kind <: NodeKind[Kind]] =
       SetValueMap[NodeId[Kind], (NodeId[Kind], AbstractionPolicy)]
  val AbstractionMap = SetValueMap
  type Node2NodeMap[Kind <: NodeKind[Kind]] = Map[NodeId[Kind], NodeId[Kind]]
  val Node2NodeMap = Map

  type NodeSet[Kind <: NodeKind[Kind]] = Map[NodeId[Kind], (String, Kind, Mutability)]
  val NodeSet = Map

  implicit def idToNode[Kind <: NodeKind[Kind]](implicit graph : AccessGraph[Kind],
                                                id : NodeId[Kind]) : AGNode[Kind] =
               graph.getNode(id)

  type Mutability = Boolean
}
import AccessGraph._

class AccessGraph[NK <: NodeKind[NK]]
( private [this] val nodeBuilder : AGNodeBuilder[NK],
  val logger : PuckLogger = PuckNoopLogger,
  private [this] val idSeed : () => Int,
  private [this] val nodesSet : NodeSet[NK],
  private [this] val usersMap : EdgeMap[NK],
  private [this] val usesMap  : EdgeMap[NK],
  private [this] val contentsMap  : EdgeMap[NK],
  private [this] val containerMap : Node2NodeMap[NK],
  private [this] val superTypesMap : EdgeMap[NK],
  private [this] val subTypesMap : EdgeMap[NK],
  private [this] val dominantUsesMap : UseDependencyMap[NK],
  private [this] val dominatedUsesMap : UseDependencyMap[NK],
  private [this] val abstractionsMap : AbstractionMap[NK],
  val recording : Recording[NK]) {

  type NodeIdT = NodeId[NK]
  type NodeType = (NodeIdT, String, NK, Mutability)
  type EdgeType = AGEdge[NK]

  def newGraph(nLogger : PuckLogger = logger,
               nNodesSet : NodeSet[NK] = nodesSet,
               nUsersMap : EdgeMap[NK] = usersMap,
               nUsesMap  : EdgeMap[NK] = usesMap,
               nContentMap  : EdgeMap[NK] = contentsMap,
               nContainerMap : Node2NodeMap[NK] = containerMap,
               nSuperTypesMap : EdgeMap[NK] = superTypesMap,
               nSubTypesMap : EdgeMap[NK] = subTypesMap,
               nDominantUsesMap : UseDependencyMap[NK] = dominantUsesMap,
               nDominatedUsesMap : UseDependencyMap[NK] = dominatedUsesMap,
               nAbstractionsMap : AbstractionMap[NK] = abstractionsMap,
               nRecording : Recording[NK] = recording) : AccessGraph[NK] =
    new AccessGraph[NK](nodeBuilder, nLogger,
                        idSeed,
                        nNodesSet, nUsersMap, nUsesMap,
                        nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
                        nDominantUsesMap, nDominatedUsesMap,
                        nAbstractionsMap, nRecording)

  def withLogger(l : PuckLogger) = newGraph(nLogger = l)
  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.InGraph(), PuckLog.Debug())
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) =
    (PuckLog.InGraph(), lvl)


  private [immutable] def addNode(id : NodeIdT, localName:String, kind: NK) : AccessGraph[NK] =
    newGraph(nNodesSet = nodesSet + (id -> (localName, kind, false)),
             nRecording = recording.addNode(id, localName, kind),
             nContainerMap = containerMap + (id -> id))




  val rootId : NodeIdT = 0
  def root = getNode(rootId)
  def isRoot(id : NodeIdT) = container(id) == id

  def addNode(localName:String, kind: NK) : AGNode[NK] = {
    val id = idSeed()
    val k = kind.create(id)
    val newG = addNode(id, localName, k)
    nodeBuilder(newG, id, localName, kind, true)
    //this.root.content_+=(n)
    //n
  }

  /*def nodes : Seq[NodeIdT] = Range(0, idSeed + 1) */
  def nodes : Iterable[AGNode[NK]] = nodesSet.values map {
    case (name, kind, mutable) => new AGNode(this, kind.node, name, kind, mutable)
  }

  def getNode(id : NodeIdT): AGNode[NK] = nodesSet get id match {
    case None => throw new AGError("illegal node request : no node has id " + id.toString)
    case Some( (name, kind, mutability) ) => nodeBuilder(this, id, name, kind, mutability)
    }

  def removeNode(id : NodeIdT) = newGraph(nNodesSet = nodesSet - id)

  def setNode(id : NodeIdT, name : String, k : NK, mutable : Boolean) = {
    val g = newGraph(nNodesSet = nodesSet + (id -> (name, k, mutable)))
    nodeBuilder(g, id, name, k, mutable)
  }

  def setKind(id : NodeIdT, k : NK) = nodesSet get id match {
    case None => throw new AGError("illegal node request : no node has id " + id.toString)
    case Some( (name, _, mutability) ) => setNode(id, name, k, mutability)


  }
  def setMutability(id : NodeIdT, mutable : Boolean) = nodesSet get id match {
    case None => throw new AGError("illegal node request : no node has id " + id.toString)
    case Some( (name, kind, _) ) => setNode(id, name, kind, mutable)
  }

  def container(contentId : NodeIdT) : NodeIdT = containerMap(contentId)
  def content(containerId: NodeIdT) : Iterable[NodeIdT] = contentsMap.getFlat(containerId)

  def addContains(containerId: NodeIdT, contentId :NodeIdT): AccessGraph[NK] =
     newGraph(nContentMap = contentsMap + (containerId, contentId),
              nContainerMap = containerMap + (contentId -> containerId))

  def contains(containerId : NodeIdT, contentId : NodeIdT) : Boolean =
      container(contentId) == containerId

  def contains_*(containerId : NodeIdT, contentId : NodeIdT) : Boolean =
      containerId == contentId || {
        val containerId2 = container(contentId)
        !(containerId2 == contentId) && contains_*(containerId, containerId2)
      }

  def addUses(userId: NodeIdT, useeId: NodeIdT): AccessGraph[NK] =
    newGraph(nUsersMap = usersMap + (useeId , userId),
             nUsesMap = usesMap + (userId, useeId))

  def uses(userId: NodeIdT, useeId: NodeIdT) : Boolean = usersMap.bind(useeId, userId)

  def users(useeId: NodeIdT) : Iterable[NodeIdT] = usersMap getFlat useeId

  def addIsa(subTypeId: NodeIdT, superTypeId: NodeIdT): AccessGraph[NK]=
    newGraph(nSubTypesMap = subTypesMap + (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap + (subTypeId, superTypeId))

  def isa(subId : NodeIdT, superId: NodeIdT): Boolean = superTypesMap.bind(subId, superId)
  def directSuperTypes(sub: NodeIdT) : Iterable[NodeIdT] = superTypesMap getFlat sub


  def dominantUses(dominatedEdge : (NodeIdT, NodeIdT)) : Iterable[(NodeIdT, NodeIdT)] =
    dominantUsesMap getFlat dominatedEdge

  def dominatedUses(dominantEdge : (NodeIdT, NodeIdT)) : Iterable[(NodeIdT, NodeIdT)] =
    dominatedUsesMap  getFlat dominantEdge

  def dominates(dominantEdge : (NodeIdT, NodeIdT),
                dominatedEdge : (NodeIdT, NodeIdT)) : Boolean =
    dominatedUses( dominantEdge ).exists(_ == dominatedEdge)

  def addUsesDependency(dominantEdge : (NodeIdT, NodeIdT),
                        dominatedEdge : (NodeIdT, NodeIdT)) : AccessGraph[NK] =
    newGraph(nDominantUsesMap = dominantUsesMap + (dominatedEdge, dominantEdge),
             nDominatedUsesMap = dominatedUsesMap + (dominantEdge, dominatedEdge))


  def abstractions(id : NodeIdT) : Iterable[(NodeIdT, AbstractionPolicy)] =
    abstractionsMap getFlat id

  def violations : Seq[EdgeType] = Seq()
}
