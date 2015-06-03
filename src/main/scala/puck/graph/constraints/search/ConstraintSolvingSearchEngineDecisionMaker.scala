package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.{NodePredicate, AbstractionPolicy, DecisionMaker}
import puck.graph.transformations.rules.CreateVarStrategy
import puck.search.SearchEngine

import scalaz._ , Scalaz._

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

  def violationTarget
  ( lg : LoggedG)
  ( k: Logged[Option[ConcreteNode]] => Unit) : Unit = {

    val graph = lg.value
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

    k(findTargets(violationsKindPriority).set(lg.written))
  }

  def abstractionKindAndPolicy
  ( lg : LoggedG, impl : ConcreteNode)
  ( k : Logged[Option[(NodeKind, AbstractionPolicy)]] => Unit) : Unit = {

    import impl.kind.{abstractionPolicies, abstractKinds}

    k(lg.map(_ =>
      if(abstractionPolicies.isEmpty)
        None
      else
        Some((abstractKinds(abstractionPolicies.head).head,
          abstractionPolicies.head))))

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


  def partition(graph : DependencyGraph) : (List[DGNode], List[NonEmptyList[DGNode]]) => List[NonEmptyList[DGNode]] = {
    case (Nil, acc) => acc
    case (hd :: tl, acc) =>
      val (same, diff) = tl.partition(n => n.kind == hd.kind)
      partition(graph)(diff, NonEmptyList[DGNode](hd, same:_*) +: acc)
  }

  def chooseContainerKind
  ( lg : LoggedG, toBeContained : DGNode)
  ( k : Logged[Option[NodeKind]] => Unit) : Unit = {
    //TODO filter instead of find !!!
    k(lg.map(_.nodeKinds.find(_.canContain(toBeContained.kind))))
  }

  def selectExistingAbstraction
  ( lg : LoggedG,
    choices : Set[(NodeId, AbstractionPolicy)])
  ( k : Logged[Option[(NodeId, AbstractionPolicy)]] => Unit) : Unit =
    k(lg.map( _ =>
      if( choices.isEmpty )
        none[(NodeId, AbstractionPolicy)]
      else some(choices.head) ) )


  def chooseNode
  ( lg : LoggedG, predicate : NodePredicate)
  ( k : LoggedG => Option[NodeId] => Unit) : Unit = {
    val graph = lg.value
    val choices = graph.concreteNodes.filter(predicate(graph,_)).toList

    choices match {
      case Nil => k(lg)(None)
      case List(n) => k(lg)(Some(n.id))
      case s =>
        val l = partition(graph)(s, List())

        val (g, cs) = l.foldLeft( (graph, List[NodeId]()) ){
          case ((g0, s0), nel)  =>
            if(nel.tail.isEmpty) (g0, nel.head.id +: s0)
            else{
              val (vn, g2) = g0.addVirtualNode(nel.toList.map(_.id).toSeq,  nel.head.kind)
              (g2, vn.id +: s0)
            }

        }

        val lg1 = lg.map(_ => g)
        searchEngine.newCurrentState(lg1,
          ConstraintSolvingNodesChoice.includeNoneChoice(k(lg1), cs))

    }
  }

  override def createVarStrategy(k : CreateVarStrategy => Unit) : Unit = ???
/*  def modifyConstraints(sources : NodeSet[Kind], target : NodeType){}*/

}
