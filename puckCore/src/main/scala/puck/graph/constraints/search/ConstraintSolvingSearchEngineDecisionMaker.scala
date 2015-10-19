package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.DecisionMaker.ChooseNodeKArg
import puck.graph.constraints.{NodePredicate, AbstractionPolicy, DecisionMaker}
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, CreateVarStrategy}
import puck.search.SearchEngine
import puck.util.Logged

import scalaz._ , Scalaz._

class ConstraintSolvingSearchEngineDecisionMaker
( val violationsKindPriority : Seq[NodeKind] )
  extends DecisionMaker {

  var searchEngine : SearchEngine[ResultT] = _


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

    import impl.kind.{abstractionPolicies, abstractionNodeKinds}

    val (needSearch, karg) =
      if(abstractionPolicies.isEmpty)
        (false, None)
      else if(abstractionPolicies.tail.isEmpty){
        val absk = abstractionNodeKinds(abstractionPolicies.head)
        if(absk.tail.isEmpty)
          (false, Some((absk.head, abstractionPolicies.head)))
        else (true, None)

      }
      else (true, None)


    if(needSearch) {
      val choices = impl.kind.abstractionChoices.map(Some(_))

      searchEngine.addState(lg,
        new ConstraintSolvingChoice[Option[(NodeKind, AbstractionPolicy)]](k,
          Set[Option[(NodeKind, AbstractionPolicy)]]() ++ choices,
          Set[Option[(NodeKind, AbstractionPolicy)]]()))
    }
    else k(karg.set(lg.written))

  }


  def partitionByKind(graph : DependencyGraph) : (List[DGNode], List[NonEmptyList[DGNode]]) => List[NonEmptyList[DGNode]] = {
    case (Nil, acc) => acc
    case (hd :: tl, acc) =>
      val (same, diff) = tl.partition(n => n.kind == hd.kind)
      partitionByKind(graph)(diff, NonEmptyList[DGNode](hd, same:_*) +: acc)
  }

  def chooseContainerKind
  ( lg : LoggedG, toBeContained : DGNode)
  ( k : Logged[Option[NodeKind]] => Unit) : Unit = {
    //TODO filter instead of find !!!
    k(lg.map(_.nodeKinds.find(_.canContain(toBeContained.kind))))
  }

  def selectExistingAbstraction
  ( lg : LoggedG,
    choices : Set[Abstraction])
  ( k : Logged[Option[Abstraction]] => Unit) : Unit =
    k(lg.map( _ =>
      if( choices.isEmpty )
        none[Abstraction]
      else some(choices.head) ) )


  def chooseNode
  ( lg : LoggedG, predicate : NodePredicate)
  ( k : ChooseNodeKArg => Unit) : Unit = {
    val graph = lg.value
    val choices = graph.concreteNodes.filter(predicate(graph,_)).toList

//    searchEngine.addState(lg,
//      ConstraintSolvingNodesChoice.includeNoneChoice(k, choices map (n => Some((graph, n.id)))))
    choices match {
      case s =>
        val l = partitionByKind(graph)(s, List())

        val cs  = l.map {
          nel =>
          if(nel.tail.isEmpty) some((graph, nel.head.id))
          else{
            val (vn, g2) = graph.addVirtualNode(nel.toList.map(_.id).toSeq,  nel.head.kind)
            some((g2, vn.id))
          }
        }

        searchEngine.addState(lg,
          ConstraintSolvingNodesChoice.includeNoneChoice(k, cs))

    }
  }

  override def createVarStrategy
  ( lg: LoggedG)
  ( k : Logged[CreateVarStrategy] => Unit) : Unit = {
    val g = lg.value
    val tmKinds = g.nodeKindKnowledge.kindOfKindType(InstanceValueDecl)
    val strategies = CreateParameter +: (tmKinds map CreateTypeMember.apply)

    searchEngine.addState(lg,
      new ConstraintSolvingChoice[CreateVarStrategy](k, strategies.toSet, Set()))

  }

}
