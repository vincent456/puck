package puck.graph.immutable

import puck.graph._
import puck.graph.constraints._
import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.constraints._
import puck.util.{Logger, PuckLog, PuckNoopLogger, PuckLogger}
import puck.graph.immutable.transformations.{RecordingComparator, Transformation, Recording}

import scala.language.existentials
import scala.util.{Failure, Success, Try}


object AccessGraph {

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

  type NodeT = (NodeId, String, NodeKind, TypeHolder, Mutability , Hook)
  type NodeIndex = Map[NodeId, NodeT]
  val NodeIndex = Map

  implicit def idToNode(implicit graph : AccessGraph, id : NodeId) : AGNode =
               graph.getNode(id)

  type Mutability = Boolean

  def areEquivalent[Kind <: NodeKind, T](initialRecord : Recording,
                      graph1 : AccessGraph,
                      graph2 : AccessGraph,
                      logger : PuckLogger = PuckNoopLogger) : Boolean = {
    val engine = new RecordingComparator(initialRecord(),graph1,graph2, logger)
    engine.explore()
    engine.finalStates.nonEmpty
  }

}
import AccessGraph._

sealed trait NodeStatus{
  val node : NodeT
}
case class Removed(node : NodeT) extends NodeStatus
case class Created(node : NodeT) extends NodeStatus

trait Hook

class AccessGraph
( private [this] val nodeBuilder : AGNodeBuilder,
  val logger : PuckLogger = PuckNoopLogger,
  private [this] val idSeed : () => Int,
  private [this] val nodesIndex : NodeIndex,
  private [this] val removedNodes : NodeIndex,
  private [this] val usersMap : EdgeMap,
  private [this] val usesMap  : EdgeMap,
  private [this] val contentsMap  : EdgeMap,
  private [this] val containerMap : Node2NodeMap,
  private [this] val superTypesMap : EdgeMap,
  private [this] val subTypesMap : EdgeMap,
  private [this] val dominated2dominantUsesMap : UseDependencyMap,
  private [this] val dominant2dominatedUsesMap : UseDependencyMap,
  private [this] val abstractionsMap : AbstractionMap,
  private [this] val constraints : ConstraintsMaps,
  val recording : Recording) {


  type NIdT = NodeId
  type NT = NodeT
  type EdgeT = AGEdge
  type GraphT = AccessGraph
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
               nDominantUsesMap : UseDependencyMap = dominated2dominantUsesMap,
               nDominatedUsesMap : UseDependencyMap = dominant2dominatedUsesMap,
               nAbstractionsMap : AbstractionMap = abstractionsMap,
               nConstraints : ConstraintsMaps = constraints,
               nRecording : Recording = recording) : AccessGraph =
    new AccessGraph(nodeBuilder, nLogger,
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


  private [immutable] def addNode(id : NIdT,
                                  localName:String,
                                  kind: NodeKind,
                                  styp: STyp,
                                  mutable : Mutability,
                                  t : Hook) : AccessGraph =
    newGraph(nNodesSet = nodesIndex + (id -> (id, localName, kind, styp, mutable, t)),
             nRemovedNodes = removedNodes - id,
             nRecording = recording.addNode(id, localName, kind, styp, mutable, t))



  val rootId : NIdT = 0
  def root = getNode(rootId)
  def isRoot(id : NIdT) = id == rootId

  def addNode(localName:String, kind: NodeKind, th : TypeHolder, mutable : Mutability = true) : (NIdT, GraphT) = {
    val id = idSeed()
    (id, addNode(id, localName, kind, th, mutable, nodeBuilder.createT(kind)))
  }

  /*def nodes : Seq[NodeIdT] = Range(0, idSeed + 1) */
  def nodes : Iterable[AGNode] = nodesIndex.values map {
    case (id, name, kind, styp, mutable, t) =>
      new AGNode(this, id, name, kind, styp, mutable, t)
  }

  private def sortedMap : Seq[(NIdT,  NT)] = nodesIndex.toSeq sortBy(_._1)

  def nodesId : Iterable[NodeId] = nodesIndex.keys
  def numNodes : Int = nodesIndex.size

  private def getNodeStatus(id : NIdT) : NodeStatus = {
    nodesIndex get id match {
      case Some(n) => Created(n)
      case None => removedNodes get id match {
        case Some(n) => Removed(n)
        case None =>
          val msg = "AccessGraph.getNode : no node has id " + id.toString
          logger.writeln(msg)(PuckLog.Error)
          logger.writeln("nodes of graph : ")(PuckLog.Error)
          logger.writeln(sortedMap.mkString("\n", "\n\t", ""))(PuckLog.Error)
          throw new AGError("illegal node request : no node has id " + id.toString)
      }
    }
  }

  def getNode(id : NIdT): AGNode = nodeBuilder(this,  getNodeStatus(id))


  def removeNode(id : NIdT) = {
    nodesIndex(id) match {
      case node @ (`id`, localName, kind, styp, mutable, t) =>
        newGraph(nNodesSet = nodesIndex - id,
          nRemovedNodes = removedNodes + (id -> node),
          nRecording = recording.removeNode(id, localName, kind, styp, mutable, t))
      case _ => throw new AGError("incoherent index left and right id are different")

    }
  }



  def setNode(n : AGNode) : GraphT =
    newGraph(nNodesSet = nodesIndex + (n.id -> (n.id, n.name, n.kind, n.styp, n.isMutable, n.t)))

  def setNode(id : NIdT, name : String, k : NodeKind, styp : STyp, mutable : Boolean, t : Hook) : GraphT =
    newGraph(nNodesSet = nodesIndex + (id -> (id, name, k, styp, mutable, t)))

  private def setRemovedNode(id : NIdT, name : String, k : NodeKind, styp : STyp, mutable : Boolean, t : Hook) : GraphT =
    newGraph(nRemovedNodes = removedNodes + (id -> (id, name, k, styp, mutable, t)))

  def setKind(id : NIdT, k : NodeKind) = getNodeStatus(id) match {
    case Created((_, name, _, styp, mutability, t)) =>  setNode(id, name, k, styp, mutability, t)
    case Removed((_, name, _, styp, mutability, t)) => setRemovedNode(id, name, k, styp, mutability, t)
  }

  def setType(id : NIdT, st : STyp) =
    getNodeStatus(id) match {
      case Created((_, name, k, _, mutability, t)) =>  setNode(id, name, k, st, mutability, t)
      case Removed((_, name, k, _, mutability, t)) => setRemovedNode(id, name, k, st, mutability, t)
    }

 def setMutability(id : NIdT, mutable : Boolean) =
   getNodeStatus(id) match {
     case Created((_, name, k, st, _, t)) =>  setNode(id, name, k, st, mutable, t)
     case Removed((_, name, k, st, _, t)) => setRemovedNode(id, name, k, st, mutable, t)
   }

 def setInternal(id : NIdT, t : Hook) =
   getNodeStatus(id) match {
     case Created((_, name, k, st, mutable, _)) =>  setNode(id, name, k, st, mutable, t)
     case Removed((_, name, k, st, mutable, _)) => setRemovedNode(id, name, k, st, mutable, t)
   }

 def addContains(containerId: NIdT, contentId :NIdT, register : Boolean = true): GraphT =
     newGraph(nContentMap = contentsMap + (containerId, contentId),
              nContainerMap = containerMap + (contentId -> containerId),
              nRecording = if(register) recording.addEdge(AGEdge.contains(containerId, contentId))
                           else recording)

  def removeContains(containerId: NIdT, contentId :NIdT, register : Boolean = true): GraphT =
      newGraph( nContentMap = contentsMap - (containerId, contentId),
                nContainerMap = containerMap - contentId,
                nRecording = if(register) recording.removeEdge(AGEdge.contains(containerId, contentId))
                             else recording )

  def addUses(userId: NIdT, useeId: NIdT, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap + (useeId , userId),
             nUsesMap = usesMap + (userId, useeId),
             nRecording = if(register) recording.addEdge(AGEdge.uses(userId, useeId))
                          else recording)

  def removeUses(userId: NIdT, useeId: NIdT, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap - (useeId , userId),
             nUsesMap = usesMap - (userId, useeId),
             nRecording = if(register) recording.removeEdge(AGEdge.uses(userId, useeId))
                          else recording)

  def addIsa(subTypeId: NIdT, superTypeId: NIdT, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap + (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap + (subTypeId, superTypeId),
            nRecording = if(register) recording.addEdge(AGEdge.isa(subTypeId,superTypeId))
                         else recording)

  def removeIsa(subTypeId: NIdT, superTypeId: NIdT, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap - (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap - (subTypeId, superTypeId),
             nRecording = if(register) recording.removeEdge(AGEdge.isa(subTypeId,superTypeId))
                          else recording)


  def addUsesDependency(dominantEdge : (NIdT, NIdT),
                        dominatedEdge : (NIdT, NIdT)) : GraphT =
    newGraph(nDominantUsesMap = dominated2dominantUsesMap + (dominatedEdge, dominantEdge),
      nDominatedUsesMap = dominant2dominatedUsesMap + (dominantEdge, dominatedEdge))

  def removeUsesDependency(dominantEdge : EdgeT,
                           dominatedEdge :EdgeT) : GraphT =
    removeUsesDependency((dominantEdge.source, dominantEdge.target),
      (dominatedEdge.source, dominatedEdge.target))

  def removeUsesDependency(dominantEdge : (NIdT, NIdT),
                           dominatedEdge : (NIdT, NIdT)) : GraphT =
    newGraph(nDominantUsesMap = dominated2dominantUsesMap - (dominatedEdge, dominantEdge),
      nDominatedUsesMap = dominant2dominatedUsesMap - (dominantEdge, dominatedEdge))

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
    val newEdge : EdgeT = new AGEdge(edge.kind, edge.source, newTarget)
    val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = newEdge.exists(this))
    newEdge.create(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeSource(edge : EdgeT, newSource : NIdT) : GraphT = {
    val g1 = edge.delete(this, register = false)
    val newEdge: EdgeT = new AGEdge(edge.kind, newSource, edge.target)
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

  def nodeKinds = nodeBuilder.kinds

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

  def usesDominating(dominatedEdge : (NIdT, NIdT)) : Iterable[(NIdT, NIdT)] =
    dominated2dominantUsesMap getFlat dominatedEdge

  def usesDominatedBy(dominantEdge : (NIdT, NIdT)) : Iterable[(NIdT, NIdT)] =
    dominant2dominatedUsesMap  getFlat dominantEdge

  def dominates(dominantEdge : (NIdT, NIdT),
                dominatedEdge : (NIdT, NIdT)) : Boolean =
    usesDominatedBy( dominantEdge ).exists(_ == dominatedEdge)

  def abstractions(id : NIdT) : Iterable[(NIdT, AbstractionPolicy)] =
    abstractionsMap getFlat id

  def violations() : Seq[EdgeT] =
    nodesIndex.keys.flatMap {n =>
      val wu = constraints.wrongUsers(this, n).map(AGEdge.uses(_,n))
      if(constraints.isWronglyContained(this, n))
         AGEdge.contains(container(n).get, n) +: wu
      else wu
    }.toSeq

  def wrongUsers(id : NIdT) : Seq[NIdT] = constraints.wrongUsers(this, id)
  def isWronglyContained(id : NIdT) = constraints.isWronglyContained(this, id)
  def interloperOf(id1 : NIdT, id2 :NIdT) = constraints.interloperOf(this, id1, id2)

  def printConstraints[V](logger : Logger[V], v : V) : Unit =
    constraints.printConstraints(this, logger, v)

 def coupling = nodes.foldLeft(0 : Double){ (acc, n) => acc + n.coupling }

  def subTree(root : NIdT) : Seq[NIdT] = {
    def aux(roots : Seq[NIdT], acc : Seq[NIdT]): Seq[NIdT] = roots match {
      case Seq() => acc
      case r +: tail =>
        val children = content(r)
        aux(children ++: tail, children ++: acc)

    }
    aux(Seq(root), Seq(root))
  }

  /*
   * High level modifications
   */

  def abstractionName(implId: NIdT, abskind : NodeKind, policy : AbstractionPolicy) : String =
    getNode(implId).name + "_" + policy

  def createNode(implId: NIdT, abskind : NodeKind, policy : AbstractionPolicy) : (NIdT, GraphT) = {
    val (id, g) = addNode(abstractionName(implId, abskind, policy), abskind, getNode(implId).styp)
    (id, g.addAbstraction(implId, (id, policy)))
  }

  def createAbstraction(implId: NIdT,
                        abskind : NodeKind ,
                        policy : AbstractionPolicy) : Try[(NIdT, GraphT)] = {
    val (absId, g) = createNode(implId, abskind, policy)

    Success((absId, policy match {
      case SupertypeAbstraction => g.addUses(implId, absId)
      case DelegationAbstraction => g.addUses(absId, implId)
    }))

  }

  def abstractionCreationPostTreatment(implId : NIdT,
                                       absId : NIdT,
                                       policy : AbstractionPolicy) : GraphT = this

  def redirectUses(oldEdge : EdgeT, newUsee : NIdT,
                   policy : RedirectionPolicy,
                   propagateRedirection : Boolean = true,
                   keepOldUse : Boolean = false ) : Try[(EdgeT, GraphT)] = {
    if(oldEdge.usee == newUsee) Success((oldEdge, this))
    else if(oldEdge.exists(this)) {

      logger.writeln("redirecting %s target to %s (%s)".format(oldEdge, newUsee, policy))

      val newUse : EdgeT = AGEdge.uses(oldEdge.user, newUsee)

      val g2 =
        if(keepOldUse)
          newUse.create(this)
        else
          oldEdge.changeTarget(this, newUsee)


      val g3 = getNode(oldEdge.user).styp match {
        case NoType => g2
        case sTyp => g2.changeType(oldEdge.user, sTyp, oldEdge.usee, newUsee)
      }

      val tryG4 = if(propagateRedirection) {
        g3.redirectPrimaryUses(oldEdge, newUsee, policy).flatMap {
          _.redirectSideUses(oldEdge, newUsee, policy)
        }
      }
      else Success(g3)

      tryG4 map {(newUse, _)}
    }
    else if (uses(oldEdge.user, newUsee)) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      logger.writeln("redirecting uses' %s target to %s (%s) : FAILURE !! %s is not used".
        format(oldEdge, newUsee, policy, oldEdge.usee))
      Success((AGEdge.uses(oldEdge.user, newUsee), this))
    }
    else if(users(oldEdge.usee).exists(_ == oldEdge.user) ||
                users(newUsee).exists(_==oldEdge.user))
        Failure(new AGError("incoherent state !!!!!!!!!!!!"))
    else
        Failure(new AGError(("redirecting uses' %s target to %s (%s)\n" +
        "!!! nor the oldUsee or the newUsee is really used !!! ").
        format(oldEdge, newUsee, policy)))

  }

  def redirectPrimaryUses(currentSideUse : EdgeT,
                          newSideUsee : NIdT,
                          policy : RedirectionPolicy,
                          propagateRedirection : Boolean = true) : Try[GraphT] = {


    logger.writeln("redirecting primary uses of side use %s (new side usee is %s) ".
      format(currentSideUse, newSideUsee))

     val primaryUses = usesDominating(currentSideUse)
     if(primaryUses.isEmpty) {
       logger.writeln("no primary uses to redirect")
       Success(this)
     }
     else{
       logger.writeln("uses to redirect:%s".format(primaryUses.mkString("\n\t", "\n\t","\n")))

       primaryUses.foldLeft[Try[GraphT]](Success(this)){
         case (g, primary0) =>
           val primary = AGEdge.uses(primary0)

           val keepOldUse = usesDominatedBy(primary0).nonEmpty //is empty if primary had only one side use

           val tryG1 : Try[GraphT] =
              g.map { _.removeUsesDependency(primary, currentSideUse)}

           val tryG2 : Try[(EdgeT, GraphT)]= tryG1.flatMap { g1 =>
              g1.redirectUses(primary,
               g1.findNewPrimaryUsee(primary.usee, newSideUsee, policy),
               policy, propagateRedirection, keepOldUse)
           }

           tryG2 map {
             case (newPrimary, g2) =>
               g2.addUsesDependency(newPrimary, (currentSideUse.user, newSideUsee))
           }

       }
     }
  }

  def findNewPrimaryUsee(currentPrimaryUsee : NIdT,
                         newSideUsee : NIdT,
                         policy : RedirectionPolicy) : NIdT = {

    logger.writeln("searching new primary usee ("+ policy + ") : currentPrimaryUsee is " +
      currentPrimaryUsee + ", new side usee " + newSideUsee)

    val newPrimaryUsee =
      policy match {
        case Move => container(newSideUsee).get
        case absPolicy : AbstractionPolicy =>
          abstractions(currentPrimaryUsee).find {
            case (node, `absPolicy`) => contains_*(node, newSideUsee)
            case _ => false
          } match {
            case Some((n, _)) => n
            case None =>
              val abstractKinds =
                      getNode(currentPrimaryUsee).kind.
                          abstractKinds(absPolicy)

              nodesId.find{node =>
                contains_*(node, newSideUsee) && {
                  abstractKinds.contains(getNode(node).kind)
                }

              } match {
                case Some(n) =>
                  logger.writeln(n + " found as primary usee")

                  n
                case None =>
                  val msg = "no correct primary abstraction found !"
                  logger.writeln(msg)(PuckLog.Error)
                  throw new RedirectionError(msg)
              }
          }
      }
    logger.writeln("new primary usee found : " + newPrimaryUsee)
    newPrimaryUsee
  }


  import puck.util.ErrorHandling.traverse

  def redirectSideUses(currentPrimaryUse: EdgeT,
                       newPrimaryUsee : NIdT,
                       policy : RedirectionPolicy) : Try[GraphT] = {
    logger.writeln("redirecting side uses of primary use %s (new primary usee is %s) ".
      format(currentPrimaryUse, newPrimaryUsee))

    val sideUses = usesDominatedBy(currentPrimaryUse)
    if(sideUses.isEmpty){
      logger.writeln("no side uses to redirect")
      Success(this)
    }
    else{
      logger.writeln("uses to redirect:%s".format(sideUses.mkString("\n\t", "\n\t","\n")))

      sideUses.foldLeft(Success(this) : Try[GraphT]){
        case (g, side0) =>
          val side = AGEdge.uses(side0)
          abstractions(side.usee).find {
            case (abs, _) => contains(newPrimaryUsee, abs)
            case _ => false
          } match {
            case None =>
              val msg = ("While redirecting primary uses %s target to %s\n" +
                "no satisfying abstraction to redirect side use %s").
                format(currentPrimaryUse, newPrimaryUsee, side)
              logger.writeln(msg)(PuckLog.Error)
              Failure(new RedirectionError(msg))
            case Some( (new_side_usee, _) ) =>

              val tryG1 : Try[GraphT] =
                  g.map(_.removeUsesDependency(currentPrimaryUse, side))

              val tryG2 : Try[(EdgeT, GraphT)] =
                  tryG1.flatMap(_.redirectUses(side, new_side_usee, policy))

               tryG2.map {
                 case (newSide, g2) =>
                  g2.addUsesDependency((currentPrimaryUse.user, newPrimaryUsee), newSide)
              }

          }
      }
    }
  }

  def moveTo(movedId : NIdT, newContainer : NIdT): Try[GraphT] = {
    val oldContainer = container(movedId)
    logger.writeln("moving " + movedId +" from " + oldContainer + " to " + newContainer)
    val g2 = changeSource(AGEdge.contains(oldContainer.get, movedId), newContainer)
    users(movedId).foldLeft(Success(g2) : Try[GraphT]){
      case (g0, userId) =>
        g0.flatMap(_.redirectPrimaryUses(AGEdge.uses(userId, movedId), movedId,
                                      Move, propagateRedirection = false))
    }
  }

  def addHideFromRootException(node : NIdT, friend : NIdT): GraphT =
      newGraph(nConstraints = constraints.addHideFromRootException(node, friend))
  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(this, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(this, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/

  def findMergingCandidate(nid : NIdT) : Option[NIdT] = None

  def findMergingCandidateIn(id : NIdT, root : NIdT) : Option[NIdT] = None

  //TODO deep merge : merge also content need to refactor find merging candidate
  //(deep merge is now done in JavaNode for interface node only)
  def merge(consumerId : NIdT, consumedId : NIdT) : GraphT = {
    val consumed = getNode(consumedId)
    val consumer = getNode(consumerId)
    val g1 = consumed.users.foldLeft(this) {
      case (g0, userId) =>
        g0.changeTarget(AGEdge.uses(userId, consumedId), consumerId)
                    .changeType(userId, getNode(userId).styp, consumedId, consumerId)
    }

    val g2 = consumed.used.foldLeft(g1) {
      case (g0, usedId) => g0.changeSource(AGEdge.uses(consumedId, usedId), consumerId)
    }

    val g3 = consumed.directSuperTypes.foldLeft(g2) {
      case (g0, stId) =>
      if(stId != consumerId) g0.changeSource(AGEdge.isa(consumedId, stId), consumerId)
      else g0.removeIsa(consumedId, stId)
    }

    val g4 = consumed.directSubTypes.foldLeft(g3) {
      case (g0, stId) =>
      if(stId != consumerId) g0.changeTarget(AGEdge.isa(stId, consumedId), consumerId)
      else g0.removeIsa(stId, consumedId)
    }

    /*(consumerId, key) is a primary uses and sidesUses(key) are the corresponding side uses */
    //val sideUses = new UsesDependencyMap(consumerId, Dominant())

    /*(other, key) is a side uses and primaryUses(key) is the corresponding primary uses */
    //val primaryUses = new UsesDependencyMap(consumerId, Dominated())


    val dominated_dominant_seq = dominated2dominantUsesMap.toSeq


    val g5 : GraphT = dominated_dominant_seq.foldLeft(g4) {
      case (g0, (( `consumedId`, sideUseeId), primUses)) =>
        primUses.foldLeft(g0) { case (g00, pUse) =>
          g00.addUsesDependency(pUse, (consumerId, sideUseeId))
             .removeUsesDependency(pUse, (consumedId, sideUseeId))
        }
      case (g0, _) => g0
    }


    val g6 = dominant2dominatedUsesMap.toSeq.foldLeft(g5) {
      case (g0, (( `consumedId`, primeUseeId), sidUses)) =>
        sidUses.foldLeft(g0) { case (g00, sUse) =>
          g00.addUsesDependency((consumerId, primeUseeId), sUse)
             .removeUsesDependency((consumedId, primeUseeId), sUse)
        }
      case (g0, _) => g0
    }


    val g7 = consumer.content.foldLeft(g6) {
      case (g0, childId) =>
        g0.findMergingCandidateIn(childId, consumedId) match {
          case Some(consumedChild) => g0.merge(childId, consumedChild)
          case None => g0
        }

    }
    val g8 = g7.content(consumedId).foldLeft(g7){
      case(g0, childId) =>
        g0.moveTo(childId, consumerId).get
          .changeType(childId, g0.getNode(childId).styp, consumedId, consumerId)
    }
    /*consumed.content.foldLeft(g6) {
      case (g0, childId) =>
        findMergingCandidateIn(childId, consumedId) match {
          case Some()
        }
        //println("moving " + childId +" into " + consumerId)

        /*val g1 = g0.changeSource(AGEdge.contains(consumedId, childId), consumerId)
        dominated_dominant_seq.foldLeft(g1){
          case (g00, ((sideUser, `childId`), primUses)) =>
            primUses.foldLeft(g00){case (g000, (pUser, `consumedId`)) =>
                val nPuser = if(pUser == consumedId) consumerId else pUser
                g000.addUsesDependency((sideUser, childId), (nPuser, consumerId))
                    .removeUsesDependency((sideUser, childId), (pUser, consumedId))
            case (g000, _) => g000
            }
          case (g00, _) => g00
        }*/
    }*/


    g8.removeContains(consumed.container.get, consumedId).removeNode(consumedId)
  }
}
