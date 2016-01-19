package puck.util

import puck.graph.{DependencyGraph, NodeIdP, SetValueMap, NodeId}
import puck.util.WeightedDirectedGraph.Weight

/**
  * Created by lorilan on 08/12/15.
  */
object WeightedDirectedGraph {
  type Weight = Int

  def fromDG
  ( g : DependencyGraph,
    isRelevantNode : (DependencyGraph, NodeId) => Boolean) : WeightedDirectedGraph = {
    def filterNodes(g : DependencyGraph) : Set[NodeId] =
      g.nodesId.foldLeft(Set[NodeId]()){
        case (s, nid) =>
          if(isRelevantNode(g, nid)) s + nid
          else s
      }

    def relevantNode(id : NodeId) : NodeId =
      if(isRelevantNode(g, id)) id
      else relevantNode(g.container_!(id))

    def relevantEdge(idp : NodeIdP) = (relevantNode(idp._1), relevantNode(idp._2))

    def extractUses(g : DependencyGraph,
                   wdg : WeightedDirectedGraph) : WeightedDirectedGraph =
      g.usesList.foldLeft(wdg){
        case (wdg0, e @ (user, used)) =>
          val re = relevantEdge(e)
          if(re._1 == re._2) wdg
          else wdg.addEdge(re)
      }

    def extractTypeUses(g : DependencyGraph,
                       wdg : WeightedDirectedGraph) : WeightedDirectedGraph = {
      g.edges.types.iterator.foldLeft(wdg){
        case (wdg0, (n, t)) =>
          val rn = relevantNode(n)
          (t.ids map relevantNode).foldLeft(wdg0){
              case (wdg1, id) =>
                if(id == rn) wdg1
                else wdg1.addEdge((rn, id))
          }
      }
    }

    val wg = new WeightedDirectedGraph(filterNodes(g))

    val wg1 = extractUses(g, wg)
    extractTypeUses(g, wg1)
  }
}
class WeightedDirectedGraph
(val nodes : Set[NodeId],
 val target2src : SetValueMap.T[NodeId, NodeId] = SetValueMap(),
 val src2target  : SetValueMap.T[NodeId, NodeId] = SetValueMap(),
 val edgesWeight : Map[NodeIdP, Weight] = Map()){

  def addEdge(e : NodeIdP)  : WeightedDirectedGraph = {
    val (src, tgt) = e
    val oldWeight = edgesWeight.getOrElse(e,0)
    new WeightedDirectedGraph(nodes,
      target2src + (tgt, src),
      src2target + (src, tgt),
      edgesWeight + (e -> (oldWeight + 1)))
  }

  def removeEdge(e : NodeIdP)  : WeightedDirectedGraph = {
    val (src, tgt) = e
    val oldWeight = edgesWeight.getOrElse(e,0)
    new WeightedDirectedGraph(nodes,
      target2src - (tgt, src),
      src2target - (src, tgt),
      edgesWeight - e )
  }
}
