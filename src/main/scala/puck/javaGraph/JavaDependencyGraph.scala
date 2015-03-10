package puck.javaGraph

import puck.graph.DependencyGraph._
import puck.graph.NodeId
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.Recording
import puck.javaGraph.nodeKind._
import puck.util.PuckLog.InJavaGraph
import puck.util.{PuckLog, PuckNoopLogger, PuckLogger}

import ShowDG._
/**
 * Created by lorilan on 29/10/14.
 */



class JavaDependencyGraph
(logger : PuckLogger = PuckNoopLogger,
 idSeed : Int,
 nodesSet : ConcreteNodeIndex,
 removedNodes : ConcreteNodeIndex,
 vNodesIndex : VirtualNodeIndex,
 vRemovedNodes : VirtualNodeIndex,
 nodes2vNodes : Nodes2VnodeMap,
 usersMap : EdgeMap,
 usesMap  : EdgeMap,
 contentsMap  : EdgeMap,
 containerMap : Node2NodeMap,
 superTypesMap : EdgeMap,
 subTypesMap : EdgeMap,
 dominantUsesMap : UseDependencyMap,
 dominatedUsesMap : UseDependencyMap,
 abstractionsMap : AbstractionMap,
 constraints : ConstraintsMaps,
 recording : Recording)
  extends DependencyGraph(logger, idSeed, nodesSet, removedNodes,
    vNodesIndex, vRemovedNodes, nodes2vNodes,
  usersMap, usesMap, contentsMap, containerMap, superTypesMap, subTypesMap,
  dominantUsesMap, dominatedUsesMap, abstractionsMap, constraints, recording){

  override def newGraph(nLogger : PuckLogger = logger,
                       nIdSeed : Int = idSeed,
               nNodesSet : ConcreteNodeIndex = nodesSet,
               nRemovedNodes : ConcreteNodeIndex = removedNodes,
               nVNodesIndex: VirtualNodeIndex = vNodesIndex,
               nVRemovedNodes :VirtualNodeIndex = vRemovedNodes,
               nNodes2vNodes : Nodes2VnodeMap = nodes2vNodes,
               nUsersMap : EdgeMap = usersMap,
               nUsesMap  : EdgeMap = usesMap,
               nContentMap  : EdgeMap = contentsMap,
               nContainerMap : Node2NodeMap = containerMap,
               nSuperTypesMap : EdgeMap = superTypesMap,
               nSubTypesMap : EdgeMap = subTypesMap,
               nDominantUsesMap : UseDependencyMap = dominantUsesMap,
               nDominatedUsesMap : UseDependencyMap = dominatedUsesMap,
               nAbstractionsMap : AbstractionMap = abstractionsMap,
               nConstraints : ConstraintsMaps = constraints,
               nRecording : Recording = recording) : JavaDependencyGraph =
    new JavaDependencyGraph(nLogger, nIdSeed,
      nNodesSet, nRemovedNodes, nVNodesIndex, nVRemovedNodes, nNodes2vNodes,
      nUsersMap, nUsesMap,
      nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
      nDominantUsesMap, nDominatedUsesMap,
      nAbstractionsMap, nConstraints, nRecording)


  implicit val defaultVerbosity = (InJavaGraph, PuckLog.Info)

  override def nodeKinds : Seq[NodeKind] = JavaNodeKind.list
  //def rootKind : JavaNodeKind = JavaRoot

  override def coupling = concreteNodes.foldLeft(0 : Double){ (acc, n) => n.kind match {
    case Package =>
      val c = Metrics.coupling(n.id, this)
      if(c.isNaN) acc
      else acc + c
    case _ => acc
  }}

  def packageNode(id : NodeId) : NodeId =
    getConcreteNode(id).kind match {
      case Package => id
      case _ => packageNode(container(id).getOrElse(throw new DGError( this.fullName(id) + "has no package")))

    }

  def concreteNodeTestPred(nid : NodeId)(pred: ConcreteNode => Boolean): Boolean =
    getNode(nid) mapConcrete (pred, false)

  override def canContain(n : ConcreteNode, other : ConcreteNode) : Boolean = {
    val id = n.id
    def noNameClash( l : Int )( cId : NIdT ) : Boolean =
      concreteNodeTestPred(cId){ c =>
        (c.kind, c.styp) match {
          case (ck: MethodKind, MethodTypeHolder(typ))=>
            c.name != other.name || typ.input.length != l
          case (ck: MethodKind, _)=> throw new DGError()
          case _ => true
        }
      }

    def implementMethod(absMethodName : String, absMethodType : Arrow[Tuple[NamedType], NamedType])(id : NodeId) : Boolean =
      content(id).exists(concreteNodeTestPred(_) { c =>
        (c.kind, c.styp) match {
          case (Method, MethodTypeHolder(typ)) => absMethodName == c.name && absMethodType == typ
          case (Method, _) => throw new DGError()
          case _ => false
        }
      })

    super.canContain(n, other) &&
      ( (other.kind, other.styp) match {
        case (AbstractMethod, MethodTypeHolder(absMethodType)) =>
          content(id).forall(noNameClash(absMethodType.input.length)) &&
            directSubTypes(id).forall {implementMethod(other.name, absMethodType)}

        case (AbstractMethod, _) => throw new DGError(other + " does not have a MethodTypeHolder")
        /* cannot have two methods with same name and same type */
        case (Method, MethodTypeHolder(typ)) =>
          content(id).forall(noNameClash(typ.input.length))
        case (Method, _) => throw new DGError(s"canContain(${showDG[NIdT](this).shows(id)}, ${showDG[NIdT](this).shows(other.id)})")
        case _ => true
      })
  }

  override def isTypeUse : DGEdge => Boolean = {
    case DGEdge(Uses, _, id) =>
      concreteNodeTestPred(id){ cn => cn.kind == Interface || cn.kind == Class}
    case _ => false
  }

  override def isTypeMemberUse : DGEdge => Boolean = {
    case DGEdge(Uses, _, id) =>
      concreteNodeTestPred(id){ cn => cn.kind match {
        case _: MethodKind | Field | Constructor => true
        case _ => false
      }
    }
    case _ => false
  }
}
