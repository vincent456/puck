package puck.graph.immutable.transformations

import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.NodeKind
import puck.search.{SearchEngine, SearchState, StateCreator}
import scala.collection.mutable
import scala.util.{Success, Try}

/**
 * Created by lorilan on 30/09/14.
 */

object MappingChoices{
  type ResMapping = Map[NodeId, Option[NodeId]]
  type NodesToMap = Seq[NodeId]

  type Kargs = (NodeId, Try[ResMapping], NodesToMap)
}

import MappingChoices.{Kargs, NodesToMap, ResMapping}

class MappingChoices(val k: Kargs => Unit,
                                             val node : NodeId,
                                             val nodesToMap : NodesToMap,
                                             val remainingChoices : mutable.Set[NodeId],
                                             val triedChoices : mutable.Set[NodeId])
  extends StateCreator[ResMapping, MappingChoices] {

  def createState (id: Int,
                   engine: SearchEngine[ResMapping],
                   prevState: Option[SearchState[ResMapping]],
                   currentResult : ResMapping,
                   choices: MappingChoices): NodeMappingState = {
    new NodeMappingState (id, engine, currentResult, choices, prevState)
  }
}

class NodeMappingState
(val id : Int,
 val engine : SearchEngine[ResMapping],
 val result : ResMapping,
 val mappingChoices: MappingChoices,
 val prevState : Option[SearchState[ResMapping]])
  extends SearchState[ResMapping] {


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

         k((c, Success(result + (node -> Some(c))), remainingNodesToMap))
       }
     }

   }
