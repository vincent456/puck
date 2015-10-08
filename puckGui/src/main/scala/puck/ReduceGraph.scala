package puck

import puck.graph.{NodeIdP, NodeId, DependencyGraph}

object ReduceGraph {

  def apply(g : DependencyGraph, roots : Set[NodeId]) : Set[NodeIdP] = {

    def containerInRoots(id : NodeId) : NodeId =
      if(roots contains id) id
      else containerInRoots(g.container_!(id))

    def f(idp : NodeIdP) = (containerInRoots(idp._1), containerInRoots(idp._2))

    def applyF(edges : List[NodeIdP]) =
      edges map f filter {case (s,t) => s != t} toSet


    applyF(g.usesList) ++ applyF(g.typeUsesList)

  }


  def isSink(edges : Set[NodeIdP])(id : NodeId) = edges forall (_._1 != id)

  def greedyCycleRemoval(nodes : Set[NodeId], edges : Set[NodeIdP]) : Set[NodeIdP] = {

    def isSource(edges : Set[NodeIdP])(id : NodeId) = edges forall (_._2 != id)

    def removeEdgesUsingNode(id : NodeId, edges : Set[NodeIdP]) : Set[NodeIdP] =
      edges filterNot (e => e._1 ==id || e._2 == id)

    def inOut(edges : Set[NodeIdP]) : Map[NodeId, Int] = {

      edges.foldLeft(Map[NodeId, Int]()) {
        case (m, (s,t)) =>
          if(s == t) m
          else {
            val i = m.getOrElse(t, 0)
            val o = m.getOrElse(s, 0)
            m + (t -> (i - 1)) + (s -> (o + 1))
          }
      }

    }

    def maxOutMinusIn(edges : Set[NodeIdP]) : NodeId = {
      val m = inOut(edges)
      m.toList.sortBy(_._2)(Ordering.Int.reverse).head._1
    }


    def orderNodes() : List[NodeId] = {
      var reverseBegin = List[NodeId]()
      var end = List[NodeId]()

      var ns: Set[NodeId] = nodes
      var es: Set[NodeIdP] = edges


      def filterNodes(acc : List[NodeId], p : Set[NodeIdP] => NodeId => Boolean) : List[NodeId] = {
        val ss = ns find p(es)
        if(ss.isEmpty) acc
        else {
          ns -= ss.get
          es = removeEdgesUsingNode(ss.get, es)
          filterNodes(ss.get :: acc, p)
        }
      }


      while(ns.nonEmpty){
        end = filterNodes(end, isSink)
        reverseBegin = filterNodes(reverseBegin, isSource)
        if(ns.nonEmpty){
          val n = maxOutMinusIn(es)
          ns -= n
          es = removeEdgesUsingNode(n, es)
          reverseBegin = n :: reverseBegin
        }
      }

      reverseBegin reverse_::: end
    }

    val order = orderNodes()

    edges filter {case (s,t) => order.indexOf(s) > order.indexOf(t)}
  }



  def longestPathLayering(nodes : Set[NodeId], edges : Set[NodeIdP]) : List[Set[NodeId]] = {
    // require (nodes, edges) is a DAG

    def edgesWithSource(nid: NodeId) = edges.filter(_._1 == nid)
    def setOfTarget(edges : Set[NodeIdP]) = edges map (_._2)

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
