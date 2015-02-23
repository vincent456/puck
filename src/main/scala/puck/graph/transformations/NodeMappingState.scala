package puck.graph
package transformations

import puck.search.{SearchEngine, SearchState, StateCreator}

import scala.collection.mutable
import scalaz.Success

/**
 * Created by lorilan on 30/09/14.
 */

object MappingChoices{
  type ResMap = Map[NodeId, (NodeKind, Option[NodeId])]
  def ResMap() = Map[NodeId, (NodeKind, Option[NodeId])]()
  type NodesToMap = Map[NodeKind, Seq[NodeId]]
  def NodesToMap() = Map[NodeKind, Seq[NodeId]]()
  type Kargs = (NodeId, Try[ResMap], NodesToMap)
}

import puck.graph.transformations.MappingChoices.{Kargs, NodesToMap, ResMap}

class MappingChoices
(val k: Kargs => Unit,
 val node : NodeId,
 val kind : NodeKind,
 val nodesToMap : NodesToMap,
 val remainingChoices : mutable.Stack[NodeId],
 val triedChoices : mutable.Stack[NodeId])
  extends StateCreator[ResMap, MappingChoices] {

  def createState (id: Int,
                   engine: SearchEngine[ResMap],
                   prevState: Option[SearchState[ResMap]],
                   currentResult : ResMap,
                   choices: MappingChoices): NodeMappingState = {
    new NodeMappingState (id, engine, currentResult, choices, prevState)
  }
}

class NodeMappingState
(val id : Int,
 val engine : SearchEngine[ResMap],
 val result : ResMap,
 val mappingChoices: MappingChoices,
 val prevState : Option[SearchState[ResMap]])
  extends SearchState[ResMap] {


     import mappingChoices._

     def triedAll = mappingChoices.remainingChoices.isEmpty

     def executeNextChoice() : Unit = {
       if(remainingChoices.nonEmpty) {
         if (engine.currentState != this)
           setAsCurrentState()

         val c = remainingChoices.pop()

         val remainingNodesToMap = nodesToMap + (kind -> (triedChoices.toSeq ++: remainingChoices.toSeq))

         triedChoices.push(c)

         k((c, Success(result + (node -> ((kind, Some(c))))), remainingNodesToMap))
       }
     }

   }
