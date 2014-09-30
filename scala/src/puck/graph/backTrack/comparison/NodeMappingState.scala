package puck.graph.backTrack.comparison

import puck.graph.{AGNode, NodeKind}
import puck.search.{SearchState, SearchEngine, StateCreator}

import scala.collection.mutable

object NodeTransfoStatus {
  def apply( i : Int) = i match {
    case 1 => Created()
    case 0 => Neuter()
    case -1 => Deleted()
    case _ => throw new Error("Illegal node transfo status value !")
  }

  type ResMapping[Kind <: NodeKind[Kind]] = Map[AGNode[Kind], (NodeTransfoStatus, Option[AGNode[Kind]])]
  type NodesToMap[Kind <: NodeKind[Kind]] = Map[NodeTransfoStatus, List[AGNode[Kind]]]

  type Kargs[Kind <: NodeKind[Kind]] = (AGNode[Kind], ResMapping[Kind], NodesToMap[Kind])
}

import NodeTransfoStatus.{Kargs, NodesToMap, ResMapping}

sealed abstract class NodeTransfoStatus
case class Created() extends NodeTransfoStatus
case class Deleted() extends NodeTransfoStatus
case class Neuter() extends NodeTransfoStatus
/**
 * Created by lorilan on 30/09/14.
 */

class AssumedChoices[Kind <: NodeKind[Kind]](val k: Kargs[Kind] => Unit,
                                             val node : AGNode[Kind],
                                             val nts : NodeTransfoStatus,
                                             val nodesToMap : NodesToMap[Kind],
                                             val remainingChoices : mutable.Set[AGNode[Kind]],
                                             val triedChoices : mutable.Set[AGNode[Kind]])
  extends StateCreator[ResMapping[Kind], AssumedChoices[Kind]] {

  def createState (id: Int,
                   engine: SearchEngine[ResMapping[Kind]],
                   prevState: Option[SearchState[ResMapping[Kind], _]],
                   currentResult : ResMapping[Kind],
                   choices: AssumedChoices[Kind]): NodeMappingState[Kind] = {
    new NodeMappingState (id, engine, currentResult, choices, prevState)
  }
}

class NodeMappingState[Kind <: NodeKind[Kind]](val id : Int,
                                                val engine : SearchEngine[ResMapping[Kind]],
                                                val result : ResMapping[Kind],
                                                val internal: AssumedChoices[Kind],
                                                val prevState : Option[SearchState[ResMapping[Kind], _]])
  extends SearchState[ResMapping[Kind], AssumedChoices[Kind]] {




     def triedAll = internal.remainingChoices.isEmpty

     def executeNextChoice() {
       import internal._
       if(remainingChoices.nonEmpty) {
         if (engine.currentState != this)
           setAsCurrentState()

         val c = remainingChoices.head
         remainingChoices.remove(c)

         val remainingNodesToMap =
           nodesToMap + (nts -> (triedChoices.toList ::: remainingChoices.toList))

         triedChoices.add(c)
         k((c, result + (node ->(nts, Some(c))), remainingNodesToMap))
       }
     }

   }
