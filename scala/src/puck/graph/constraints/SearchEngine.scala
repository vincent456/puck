package puck.graph.constraints

import puck.graph.backTrack.Recording
import puck.graph.{AGNode, NodeKind, AccessGraph}
import puck.javaAG.JavaNodeKind.{Interface, Class, Constructor, Field}

/**
 * Created by lorilan on 01/07/14.
 */

class ProblemState(r : Recording,
                   sdm : SearchEngineDecisionMaker){

}


class SearchEngine(graph : AccessGraph,
                   solverBuilder : SolverBuilder) {
  var currentState : ProblemState



}


class SearchEngineDecisionMaker(val graph : AccessGraph,
                                 ) extends DecisionMaker{


  override def toString = "Default Strategy"

  val violationsKindPriority = List[NodeKind](Field(), Constructor(),
    Class(), Interface())

  def violationTarget : Option[AGNode] = {
    def aux (priorities : List[NodeKind]) : Option[AGNode] = priorities match {
      case topPriority :: tl => graph.iterator.find{ (n : AGNode) =>
        n.kind == topPriority && (n.wrongUsers.nonEmpty ||
          n.isWronglyContained)
      } match {
        case None => aux(tl)
        case res => res
      }
      case List() => graph.iterator.find{ n => n.wrongUsers.nonEmpty ||
        n.isWronglyContained }
    }

    aux(violationsKindPriority)
  }


  def abstractionKindAndPolicy(impl : AGNode) = {
    val policy = impl.kind.abstractionPolicies.head
    (impl.kind.abstractKinds(policy).head, policy)
  }

  def chooseNode(context : String)(predicate : AGNode => Boolean) : Option[AGNode] = {
    println(context)
    val found = graph.iterator.find(predicate)
    println("found : " + found)
    found
  }

  def modifyConstraints(sources : NodeSet, target : AGNode){}

}