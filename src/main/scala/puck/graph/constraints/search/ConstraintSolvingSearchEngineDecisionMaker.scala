package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.{NodePredicate, AbstractionPolicy, DecisionMaker}
import puck.graph.transformations.rules.CreateVarStrategy
import puck.search.SearchEngine

object ConstraintSolving {
  type NodeChoice = ConstraintSolvingNodesChoice
  type AbsChoice = ConstraintSolvingAbstractionChoice
}

class ConstraintSolvingSearchEngineDecisionMaker
( val violationsKindPriority : Seq[NodeKind] )
  extends DecisionMaker {

  var searchEngine : SearchEngine[ResultT] = _
  //def initialState = new CSInitialSearchState(this, solverBuilder(graph, this))



  /*def violationTarget(graph : GraphT)
                     (k: Option[NIdT] => Unit) : Unit = {

    implicit val g = graph

    def findTargets(l : Seq[NodeKind]) : Iterator[DGNode] =  l match {
      case topPriority :: tl =>
        val it = graph.nodes.iterator filter { n =>
          n.kind == topPriority && (g.wrongUsers(n.id).nonEmpty ||
            g.isWronglyContained(n.id))
        }
        if(it.hasNext) it
        else findTargets(tl)

      case Seq() => graph.nodes.iterator filter { n => g.wrongUsers(n.id).nonEmpty ||
        g.isWronglyContained(n.id) }
    }

    val targets = (findTargets(violationsKindPriority) map (_.id)).toSeq

    lazy val targetsChoice = ConstraintSolvingNodesChoice(k, targets)

    targets match {
      case Seq() => k(None) // will call doMerge

      case Seq(oneTarget) =>
        k(Some(oneTarget))
      case _ => //more than one target
        newCurrentState((graph, graph.recording), targetsChoice)

    }
  }*/

  def violationTarget(graph : DependencyGraph)
                     (k: Option[ConcreteNode] => Unit) : Unit = {


    def findTargets(l : Seq[NodeKind]) : Option[ConcreteNode] =  l match {
      case topPriority :: tl =>
        graph.concreteNodes.iterator find { n =>
          n.kind == topPriority && (graph.wrongUsers(n.id).nonEmpty ||
            graph.isWronglyContained(n.id))
        } match {
          case None => findTargets(tl)
          case st @ Some(_) => st
        }

      case Seq() => graph.concreteNodes find { n => graph.wrongUsers(n.id).nonEmpty ||
        graph.isWronglyContained(n.id) }
    }

    k(findTargets(violationsKindPriority))
  }

  def abstractionKindAndPolicy(graph : DependencyGraph, impl : ConcreteNode)
                              (k : Option[(NodeKind, AbstractionPolicy)] => Unit) : Unit = {

    import impl.kind.{abstractionPolicies, abstractKinds}

    if(abstractionPolicies.isEmpty) {
      k(None)
    }
    else{
        k(Some((abstractKinds(abstractionPolicies.head).head,
          abstractionPolicies.head)))
    }

    /*val (needSearch, karg) =
      if(abstractionPolicies.isEmpty) {
        (false, None)
    }
    else if(abstractionPolicies.tail.isEmpty){
      val absk = abstractKinds(abstractionPolicies.head)
      if(absk.tail.isEmpty) {
        (false, Some((absk.head, abstractionPolicies.head)))
      }
      else (true, None)

    }
    else (true, None)


    if(needSearch) {
      val choices = abstractionPolicies.map { p => abstractKinds(p).map { kind => Some((kind, p)) }}.flatten

      newCurrentState((graph, graph.recording),
        new ConstraintSolvingAbstractionChoice(k,
          Set[Option[(NodeKind, AbstractionPolicy)]]() ++ choices,
          Set[Option[(NodeKind, AbstractionPolicy)]]()))
    }
    else k(karg)*/

  }


  def partition(graph : DependencyGraph) : (List[DGNode], List[List[DGNode]]) => List[List[DGNode]] = {
    case (Seq(), acc) => acc
    case (hd :: tl, acc) =>
      val (same, diff) = tl.partition(n => n.kind == hd.kind)
      partition(graph)(diff, (hd :: same) +: acc)
  }

  def chooseContainerKind(graph : DependencyGraph, toBeContained : DGNode)
                         (k : Option[NodeKind] => Unit) : Unit = {
    //TODO filter instead of find !!!
    k(graph.nodeKinds.find(_.canContain(toBeContained.kind)))
  }

  def selectExistingAbstraction
  ( graph : DependencyGraph, choices : Set[(NodeId, AbstractionPolicy)])
  ( k : Option[(NodeId, AbstractionPolicy)] => Unit) : Unit =
    if( choices.isEmpty ) k(None)
    else k(Some(choices.head))

  def chooseNode(graph : DependencyGraph, predicate : NodePredicate)
                (k : DependencyGraph => Option[NodeId] => Unit) : Unit = {
    val choices = graph.concreteNodes.filter(predicate(graph,_)).toList


    choices match {
      case List() => k(graph)(None)
      case List(n) => k(graph)(Some(n.id))
      case s =>
        val l = partition(graph)(s, List())

        val (g, cs) = l.foldLeft( (graph, Seq[NodeId]()) ){case ((g, s0), ns) =>
            val (vn, g2) = g.addVirtualNode(ns.map(_.id).toSeq,  ns.head.kind)
          (g2, vn.id +: s0)
        }

        searchEngine.newCurrentState(g,
          ConstraintSolvingNodesChoice.includeNoneChoice(k(g), cs))

    }
  }

  override def createVarStrategy(k : CreateVarStrategy => Unit) : Unit = ???
/*  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}*/

}
