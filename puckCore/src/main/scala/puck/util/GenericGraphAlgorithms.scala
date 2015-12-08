package puck.util

import puck.graph.{Uses, DependencyGraph, NodeId, NodeIdP}


object GreedyCycleRemover {
 trait Helper[G] {
   def nodes(g : G) : Set[NodeId]
   def edges(g : G) : Iterator[NodeIdP]

   def isSink(g: G)(nid: NodeId): Boolean

   def isSource(g: G)(nid: NodeId): Boolean

   def inOut(g : G) : Map[NodeId, Int]

   def removeEdgesUsingNode(id: NodeId, g: G): G
 }

  object DGHelper extends Helper[DependencyGraph] {
    def nodes(g: DependencyGraph) = g.nodesId.toSet
    def edges(g : DependencyGraph) : Iterator[NodeIdP] =  g.edges.usedMap.iterator

    //HYPOTHESIS : no more type edges have been replaced by regular uses
    def isSink(g : DependencyGraph)(id : NodeId) =
      g.edges.usedMap get id isEmpty

    def isSource(g : DependencyGraph)(id : NodeId) =
      g.edges.userMap get id isEmpty

    def removeEdgesUsingNode(id : NodeId, g : DependencyGraph) : DependencyGraph = {
      val useds = g usedBy id
      val users = g usersOf id
      val g1 = useds.foldLeft(g)((g0, used) => g0.removeEdge(Uses(id, used)))
      users.foldLeft(g1)((g0, user) => g0.removeEdge(Uses(user, id)))
    }

    def inOut(g : DependencyGraph) : Map[NodeId, Int] = {
      val mOut = g.edges.usedMap.content mapValues ((s : Set[NodeId]) => s.size)
      val mIn = g.edges.userMap.content mapValues ((s : Set[NodeId]) => s.size)

      mIn.foldLeft(mOut){
        case (m, (id, inValue)) =>
          val outValue = m.getOrElse(id, 0)
          m + (id -> (outValue - inValue))
      }
    }
  }

  object WDGHelper extends Helper[WeightedDirectedGraph] {
    def nodes(g: WeightedDirectedGraph) = g.nodes
    def edges(g : WeightedDirectedGraph) : Iterator[NodeIdP] =  g.src2target.iterator

    //HYPOTHESIS : no more type edges have been replaced by regular uses
    def isSink(g : WeightedDirectedGraph)(id : NodeId) =
      g.src2target get id isEmpty

    def isSource(g : WeightedDirectedGraph)(id : NodeId) =
      g.target2src get id isEmpty

    def removeEdgesUsingNode(id : NodeId, g : WeightedDirectedGraph) : WeightedDirectedGraph = {
      val useds = g.src2target getFlat id
      val users = g.target2src getFlat id
      val g1 = useds.foldLeft(g)((g0, used) => g0.removeEdge(Uses(id, used)))
      users.foldLeft(g1)((g0, user) => g0.removeEdge(Uses(user, id)))
    }

    def inOut(g : WeightedDirectedGraph) : Map[NodeId, Int] = {
      val mOut = g.src2target.content.foldLeft(Map[NodeId, Int]()) {
        case (m, (src , tgts)) =>

          val out =
            tgts.foldLeft(0)((i, tgt) => i + g.edgesWeight((src,tgt)))

          m + (src -> out)

      }

      val mIn = g.target2src.content.foldLeft(Map[NodeId, Int]()) {
        case (m, (tgt , srcs)) =>

          val in =
            srcs.foldLeft(0)((i, src) => i + g.edgesWeight((src, tgt)))

          m + (tgt -> in)

      }

      mIn.foldLeft(mOut){
        case (m, (id, inValue)) =>
          val outValue = m.getOrElse(id, 0)
          m + (id -> (outValue - inValue))
      }
    }
  }

  object NaiveHelper extends Helper[(Set[NodeId],  Set[NodeIdP])]{
    def nodes(g: (Set[NodeId], Set[(NodeId, NodeId)])): Set[NodeId] = g._1

    def isSource(g: (Set[NodeId], Set[(NodeId, NodeId)]))(nid: NodeId): Boolean =
      edges(g) forall (_._2 != nid)

    def edges(g: (Set[NodeId], Set[(NodeId, NodeId)])): Iterator[(NodeId, NodeId)] =
      g._2.iterator

    def inOut(g: (Set[NodeId], Set[(NodeId, NodeId)])): Map[NodeId, NodeId] =
      edges(g).foldLeft(Map[NodeId, Int]()) {
          case (m, (s,t)) =>
            if(s == t) m
            else {
              val i = m.getOrElse(t, 0)
              val o = m.getOrElse(s, 0)
              m + (t -> (i - 1)) + (s -> (o + 1))
            }
        }


    def isSink(g: (Set[NodeId], Set[(NodeId, NodeId)]))(nid: NodeId): Boolean =
      edges(g) forall (_._1 != nid)

    def removeEdgesUsingNode(id: NodeId, g: (Set[NodeId], Set[(NodeId, NodeId)])): (Set[NodeId], Set[(NodeId, NodeId)]) =
      (g._1, g._2 filterNot (e => e._1 ==id || e._2 == id))
  }
}

class GreedyCycleRemover[G]
( helper : GreedyCycleRemover.Helper[G]) {

  import helper._
  def maxOutMinusIn(g : G) : NodeId = {
    val m = helper.inOut(g)
    m.toList.sortBy(_._2)(Ordering.Int.reverse).head._1
  }


  def orderNodes(g: G) : List[NodeId] = {
    var reverseBegin = List[NodeId]()
    var end = List[NodeId]()

    var ns: Set[NodeId] = nodes(g)
    var graph : G = g

    def filterNodes(acc : List[NodeId], p : G => NodeId => Boolean) : List[NodeId] = {
      val ss = ns find p(graph)
      if(ss.isEmpty) acc
      else {
        ns -= ss.get
        graph = removeEdgesUsingNode(ss.get, graph)
        filterNodes(ss.get :: acc, p)
      }
    }

    while(ns.nonEmpty){
      end = filterNodes(end, isSink)
      reverseBegin = filterNodes(reverseBegin, isSource)
      if(ns.nonEmpty){
        val n = maxOutMinusIn(graph)
        ns -= n
        graph = removeEdgesUsingNode(n, graph)
        reverseBegin = n :: reverseBegin
      }
    }

    reverseBegin reverse_::: end
  }

  //algorithm of Eades et al. from "A fast and effective heuristic for the feedback arc set problem" 93
  def edgesToRemove(g : G): Iterator[NodeIdP] ={
    val order = orderNodes(g)
    edges(g) filter {case (s,t) => order.indexOf(s) > order.indexOf(t)}
  }


}

object GenericGraphAlgorithms {


  def reduceGraphG(g : DependencyGraph, isRelevantNode : (DependencyGraph, NodeId) => Boolean) : DependencyGraph = {

    def filterNodes(g : DependencyGraph) : DependencyGraph =
      g.nodesId.foldLeft(g){
        case (g0, nid) =>
          if(isRelevantNode(g0, nid)) g0
          else g0.removeNode(nid)._2
      }

    def relevantNode(id : NodeId) : NodeId =
      if(isRelevantNode(g, id)) id
      else relevantNode(g.container_!(id))

    def relevantEdge(idp : NodeIdP) = (relevantNode(idp._1), relevantNode(idp._2))

    def filterUses(g : DependencyGraph) : DependencyGraph =
      g.edges.usedMap.iterator.foldLeft(g){
        case (g0, e @ (user, used)) =>
          val re = relevantEdge(e)
          if( re == e) g0
          else{
            val g1 = g0.removeEdge(Uses(user, used))
            if(re._1 == re._2) g1
            else g1.addUses(re._1, re._2)
          }
      }

    def filterTypeUses(g : DependencyGraph) : DependencyGraph = {
      g.edges.types.iterator.foldLeft(g){
        case (g0, (n, t)) =>
          if(isRelevantNode(g0, n) &&
              t.ids.forall(isRelevantNode(g0,_)))
            g0
          else {
            val rn = relevantNode(n)
            (t.ids map relevantNode).foldLeft(g0.setType(n, None)){
              case (g1, id) =>
                if(id == rn) g1
                else g1.addEdge(Uses(rn, id))
            }
          }
      }
    }

    (filterNodes _ andThen filterUses andThen filterTypeUses)(g)

  }


  def reduceGraph(g : DependencyGraph, isRelevantNode : (DependencyGraph, NodeId) => Boolean) : Set[NodeIdP] = {

    def relevantNode(id : NodeId) : NodeId =
      if(isRelevantNode(g, id)) id
      else relevantNode(g.container_!(id))

    def f(idp : NodeIdP) = (relevantNode(idp._1), relevantNode(idp._2))

    def applyF(edges : List[NodeIdP]) =
      edges map f filter {case (s,t) => s != t} toSet

    applyF(g.usesList) ++ applyF(g.typeUsesList)

  }

  def reduceGraph(g : DependencyGraph, roots : Set[NodeId]) : Set[NodeIdP] =
      reduceGraph (g, (g0, id) =>roots contains id)

  //longest path algorithm in "Handbook of Graph Drawing and Visualization" Chap 13 - p.420
  def longestPathLayering(nodes : Set[NodeId], edges : Set[NodeIdP]) : List[Set[NodeId]] = {
    // require (nodes, edges) is a DAG

    def edgesWithSource(nid: NodeId) = edges.filter(_._1 == nid)
    def setOfTarget(edges : Set[NodeIdP]) = edges map (_._2)

    def isSink(edges : Set[NodeIdP])(id : NodeId) = edges forall (_._1 != id)

    var current_layer = nodes filter isSink(edges)
    var layering = List[Set[NodeId]](current_layer)
    var remainingNodes = nodes -- current_layer
    var nodesAssignedInOtherLayers = current_layer
    current_layer = Set()

    while(remainingNodes.nonEmpty){
      remainingNodes find (nid => setOfTarget(edgesWithSource(nid)) forall nodesAssignedInOtherLayers.contains ) match {
        case Some(nid) =>
          current_layer += nid
          remainingNodes -= nid
        case None =>
          layering ::=  current_layer
          nodesAssignedInOtherLayers ++= current_layer
          current_layer = Set()

      }
    }

    layering
  }

}
