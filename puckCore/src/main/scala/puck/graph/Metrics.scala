package puck.graph

object Metrics {


  def outgoingDependencies(root : NodeId, graph : DependencyGraph) : Set[DGEdge] ={

    def aux(id : NodeId, acc0 : Set[DGEdge]) : Set[DGEdge]= {
      val acc1 = graph.usedBy(id).foldLeft(acc0){
        (acc, usee) =>
          if(graph.contains_*(root,usee)) acc
          else acc + DGEdge.UsesK(id, usee)
      }
      graph.content(id).foldLeft(acc1){(acc, child) => aux(child, acc)}
    }

    aux(root, Set[DGEdge]())
  }



  def incomingDependencies(root : NodeId, graph : DependencyGraph) : Set[DGEdge] = {
    def aux(id: NodeId, acc0 : Set[DGEdge]) : Set[DGEdge]= {
      val acc1 = graph.usersOf(id).foldLeft(acc0){
        (acc, user) =>
          if(graph.contains_*(root, user)) acc
          else acc + DGEdge.UsesK(user, id)
      }
      graph.content(id).foldLeft(acc1){(acc, child) => aux(child, acc)}
    }

    aux(root,  Set[DGEdge]())
  }


  def internalDependencies(root : NodeId, graph : DependencyGraph) : Set[DGEdge] = {
    def aux(root : NodeId, id : NodeId, acc0 : Set[DGEdge]) : Set[DGEdge]= {
      val acc1 = graph.usedBy(id).foldLeft(acc0) {
        (acc, usee) =>
          if (graph.contains_*(root,usee))
            acc + DGEdge.UsesK(id, usee)
          else acc
      }
      graph.content(id).foldLeft(acc1){(acc, child) => aux(root,child, acc)}
    }
    aux(root, root, Set[DGEdge]())
  }

  def provides(provider : NodeId, other : NodeId, graph : DependencyGraph) = {
    val these = graph.subTree(provider)
    val others = graph.subTree(other)

    these.exists { t =>
      others.exists { o =>
        graph.uses(o, t) &&
          !(graph.contains_*(other, o) && graph.contains_*(other, t))
      }
    }
  }

  private def connection(node : DGNode, p : DGNode => Boolean, graph : DependencyGraph) = {
    graph.nodes.foldLeft(Set[NodeId]()){ (acc, n) =>
      if(n.id == node.id || n.kind != node.kind) acc
      else if(p(n)) acc + n.id
      else acc
    }
  }

  def providers(id :NodeId, graph : DependencyGraph) : Set[NodeId] =
    connection(graph.getNode(id), n => provides(n.id, id, graph), graph)

  def clients(id :NodeId, graph : DependencyGraph) : Set[NodeId] =
    connection(graph.getNode(id), n => provides(id, n.id, graph), graph)

  def relativeCohesion(userTree : NodeId, candidate : NodeId, graph : DependencyGraph) : Double = {
    val usedElts =outgoingDependencies(userTree, graph).map(_.used)
    relativeCohesion(usedElts, candidate, graph)
  }

  def relativeCohesion(usedElts : Set[NodeId], candidate : NodeId, graph : DependencyGraph) : Double = {
    val subTree = graph.subTree(candidate).toSet
    usedElts.intersect(subTree).size.toDouble/usedElts.union(subTree).size
  }

  def cohesion(root : NodeId, graph : DependencyGraph) : Double = {
    val intd = internalDependencies(root, graph).size
    val incOutSum = outgoingDependencies(root, graph).size + incomingDependencies(root, graph).size
    intd.toDouble / (incOutSum + intd).toDouble
  }

  def coupling(root : NodeId, graph : DependencyGraph) : Double = {
    val dependencies =
      outgoingDependencies(root, graph).size +
      incomingDependencies(root, graph).size +
        internalDependencies(root, graph).size
    1 - (providers(root, graph) ++ clients(root, graph)).size.toDouble / dependencies.toDouble
  }

}
