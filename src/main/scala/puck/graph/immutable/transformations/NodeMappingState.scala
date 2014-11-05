package puck.graph.immutable.transformations

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.NodeKind
import puck.search.{SearchEngine, SearchState, StateCreator}
import scala.collection.mutable

/**
 * Created by lorilan on 30/09/14.
 */

object MappingChoices{
  type ResMapping[Kind <: NodeKind[Kind]] = Map[NodeId[Kind], Option[NodeId[Kind]]]
  type NodesToMap[Kind <: NodeKind[Kind]] = Seq[NodeId[Kind]]

  type Kargs[Kind <: NodeKind[Kind]] = (NodeId[Kind], ResMapping[Kind], NodesToMap[Kind])
}

import MappingChoices.{Kargs, NodesToMap, ResMapping}

class MappingChoices[Kind <: NodeKind[Kind]](val k: Kargs[Kind] => Unit,
                                             val node : NodeId[Kind],
                                             val nodesToMap : NodesToMap[Kind],
                                             val remainingChoices : mutable.Set[NodeId[Kind]],
                                             val triedChoices : mutable.Set[NodeId[Kind]])
  extends StateCreator[ResMapping[Kind], MappingChoices[Kind]] {

  def createState (id: Int,
                   engine: SearchEngine[ResMapping[Kind]],
                   prevState: Option[SearchState[ResMapping[Kind]]],
                   currentResult : ResMapping[Kind],
                   choices: MappingChoices[Kind]): NodeMappingState[Kind] = {
    new NodeMappingState (id, engine, currentResult, choices, prevState)
  }
}

class NodeMappingState[Kind <: NodeKind[Kind]](val id : Int,
                                                val engine : SearchEngine[ResMapping[Kind]],
                                                val result : ResMapping[Kind],
                                                val mappingChoices: MappingChoices[Kind],
                                                val prevState : Option[SearchState[ResMapping[Kind]]])
  extends SearchState[ResMapping[Kind]] {


     import mappingChoices._

     def triedAll = mappingChoices.remainingChoices.isEmpty

     def executeNextChoice() {
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
