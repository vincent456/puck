package puck.graph
package transformations

import puck.graph.transformations.MappingChoices.{ResMap, NodesToMap, Kargs}
import puck.search.{SearchEngine, SearchState, StateCreator}
import puck.util.Logged

import scala.collection.mutable

object MappingChoices{
  type ResMap = Map[NodeId, (NodeKind, Option[NodeId])]
  def ResMap() = Map[NodeId, (NodeKind, Option[NodeId])]()
  type NodesToMap = Map[NodeKind, Seq[NodeId]]
  def NodesToMap() = Map[NodeKind, Seq[NodeId]]()
  type Kargs = (NodeId, LoggedTry[ResMap], NodesToMap)
}

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
                   currentResult : Logged[ResMap],
                   choices: MappingChoices): NodeMappingState = {
    new NodeMappingState (id, currentResult, choices, prevState)
  }
}

class NodeMappingState
(val id : Int,
 val loggedResult : Logged[ResMap],
 val mappingChoices: MappingChoices,
 val prevState : Option[SearchState[ResMap]])
  extends SearchState[ResMap] {


     import mappingChoices._

     def triedAll = mappingChoices.remainingChoices.isEmpty

     def executeNextChoice(engine : SearchEngine[ResMap]) : Unit = {
       if(remainingChoices.nonEmpty) {

         val c = remainingChoices.pop()

         val remainingNodesToMap = nodesToMap + (kind -> (triedChoices.toSeq ++: remainingChoices.toSeq))

         triedChoices.push(c)

         k((c, loggedResult.map(_ + (node -> ((kind, Some(c))))).toLoggedTry, remainingNodesToMap))
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
                   currentResult : Logged[ResMap],
                   choices: StackSaver): StackSaverState = {
    new StackSaverState (id, currentResult, choices, prevState)
  }
}

class StackSaverState
( val id : Int,
  val loggedResult : Logged[ResMap],
  val stackSaver: StackSaver,
  val prevState : Option[SearchState[ResMap]])
  extends SearchState[ResMap] {

  import stackSaver._

  var executed = false

  def triedAll = executed

  def executeNextChoice(engine : SearchEngine[ResMap]) : Unit = {
    executed = true
    k((mappedNode, loggedResult.toLoggedTry, nodesToMap))
  }

}