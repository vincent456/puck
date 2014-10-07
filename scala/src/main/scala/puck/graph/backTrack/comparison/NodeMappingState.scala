package puck.graph.backTrack.comparison

import puck.graph.{AGNode, NodeKind}
import puck.search.{SearchState, SearchEngine, StateCreator}

import scala.collection.mutable


/**
 * Created by lorilan on 30/09/14.
 */

object AssumedChoices{
  type ResMapping[Kind <: NodeKind[Kind]] = Map[AGNode[Kind], Option[AGNode[Kind]]]
  type NodesToMap[Kind <: NodeKind[Kind]] = List[AGNode[Kind]]

  type Kargs[Kind <: NodeKind[Kind]] = (AGNode[Kind], ResMapping[Kind], NodesToMap[Kind])
}

import AssumedChoices.{Kargs, NodesToMap, ResMapping}

class AssumedChoices[Kind <: NodeKind[Kind]](val k: Kargs[Kind] => Unit,
                                             val node : AGNode[Kind],
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

         val remainingNodesToMap = triedChoices.toList ::: remainingChoices.toList

         triedChoices.add(c)

         k((c, result + (node -> Some(c)), remainingNodesToMap))
       }
     }

   }
