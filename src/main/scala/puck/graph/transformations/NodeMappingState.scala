package puck.graph
package transformations

import puck.search.{SearchEngine, SearchState, StateCreator}

import scala.collection.mutable
import scalaz.{\/-, Success}

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
                   prevState: Option[SearchState[ResMap]],
                   currentResult : ResMap,
                   choices: MappingChoices): NodeMappingState = {
    new NodeMappingState (id, currentResult, choices, prevState)
  }
}

class NodeMappingState
(val id : Int,
 val result : ResMap,
 val mappingChoices: MappingChoices,
 val prevState : Option[SearchState[ResMap]])
  extends SearchState[ResMap] {


     import mappingChoices._

     def triedAll = mappingChoices.remainingChoices.isEmpty

     def executeNextChoice(engine : SearchEngine[ResMap]) : Unit = {
       if(remainingChoices.nonEmpty) {
         if (engine.currentState != this)
           setAsCurrentState(engine)

         val c = remainingChoices.pop()

         val remainingNodesToMap = nodesToMap + (kind -> (triedChoices.toSeq ++: remainingChoices.toSeq))

         triedChoices.push(c)

         k((c, \/-(result + (node -> ((kind, Some(c))))), remainingNodesToMap))
       }
     }

   }


class StackSaver
(val k: Kargs => Unit,
 val mappedNode : NodeId,
 val nodesToMap : NodesToMap)
  extends StateCreator[ResMap, StackSaver] {

  def createState (id: Int,
                   prevState: Option[SearchState[ResMap]],
                   currentResult : ResMap,
                   choices: StackSaver): StackSaverState = {
    new StackSaverState (id, currentResult, choices, prevState)
  }
}

class StackSaverState
( val id : Int,
  val result : ResMap,
  val stackSaver: StackSaver,
  val prevState : Option[SearchState[ResMap]])
  extends SearchState[ResMap] {

  import stackSaver._

  var executed = false

  def triedAll = executed

  def executeNextChoice(engine : SearchEngine[ResMap]) : Unit = {
    executed = true
    k((mappedNode, \/-(result), nodesToMap))
  }

}