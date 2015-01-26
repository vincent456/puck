package puck.javaAG

import puck.graph.DependencyGraph._
import puck.graph.NodeId
import puck.graph._
import puck.graph.constraints.{ConstraintsMaps, RedirectionPolicy, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.Recording
import puck.javaAG.nodeKind._
import puck.util.PuckLog.InJavaGraph
import puck.util.{PuckLog, PuckNoopLogger, PuckLogger}

import scala.util.{Try, Failure, Success}

/**
 * Created by lorilan on 29/10/14.
 */



class JavaDependencyGraph
(program : AST.Program,
 logger : PuckLogger = PuckNoopLogger,
 idSeed : () => Int,
 nodesSet : NodeIndex,
 removedNodes : NodeIndex,
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
  extends DependencyGraph(JavaNode, logger, idSeed, nodesSet, removedNodes,
  usersMap, usesMap, contentsMap, containerMap, superTypesMap, subTypesMap,
  dominantUsesMap, dominatedUsesMap, abstractionsMap, constraints, recording){

  override def newGraph(nLogger : PuckLogger = logger,
               nNodesSet : NodeIndex = nodesSet,
               nRemovedNodes : NodeIndex = removedNodes,
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
               nRecording : Recording = recording) : DependencyGraph =
    new JavaDependencyGraph(program, nLogger, idSeed,
      nNodesSet, nRemovedNodes, nUsersMap, nUsesMap,
      nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
      nDominantUsesMap, nDominatedUsesMap,
      nAbstractionsMap, nConstraints, nRecording)


  implicit val defaultVerbosity = (InJavaGraph, PuckLog.Info)

  override def coupling = nodes.foldLeft(0 : Double){ (acc, n) => n.kind match {
    case Package =>
      val c = n.coupling(this)
      if(c.isNaN) acc
      else acc + c
    case _ => acc
  }}

  def packageNode(id : NodeId) : NodeId =
    getNode(id).kind match {
      case Package => id
      case _ => packageNode(container(id).getOrElse(throw new AGError( this.fullName(id) + "has no package")))

    }

  override def canContain(id : NIdT, otherId : NIdT) : Boolean = {

    val other = getNode(otherId)
    def noNameClash( l : Int )( cId : NIdT ) : Boolean = {
      val c = getNode(cId)
      (c.kind, c.styp) match {
        case (ck: MethodKind, MethodTypeHolder(typ))=>
          c.name != other.name || typ.input.length != l
        case (ck: MethodKind, _)=> throw new AGError()
        case _ => true
      }
    }

    super.canContain(id, otherId) &&
      ( (other.kind, other.styp) match {
        case (AbstractMethod, MethodTypeHolder(absTyp)) =>
          /*
            All subtypes must implement the method
           */
          content(id).forall(noNameClash(absTyp.input.length)) &&
            directSubTypes(id).forall { id =>
              content(id).exists { cid =>
                val c = getNode(cid)
                (c.kind, c.styp) match {
                  case (Method, MethodTypeHolder(typ)) => other.name == c.name && absTyp == typ
                  case (Method, _) => throw new AGError()
                  case _ => false
                }
              }
            }
        case (AbstractMethod, _) => throw new AGError(other + " does not have a MethodTypeHolder")
        /* cannot have two methods with same name and same type */
        case (Method, MethodTypeHolder(typ)) =>
          content(id).forall(noNameClash(typ.input.length))
        case (Method, _) => throw new AGError()
        case _ => true
      })
  }
}
