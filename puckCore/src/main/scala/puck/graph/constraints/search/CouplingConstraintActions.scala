package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search.SearchControl


class BlindControl
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[SResult]
  with ActionGenerator {


  def findTargets(graph : DependencyGraph,
                  l : Seq[NodeKind] = violationsKindPriority.toStream) : Seq[ConcreteNode] =  l match {
    case topPriority +: tl =>
      val tgts = graph.concreteNodes.toStream filter { n =>
        n.kind == topPriority && ((graph, constraints).wrongUsers(n.id).nonEmpty ||
          (graph, constraints).isWronglyContained(n.id))
      }
      if(tgts.nonEmpty) tgts
      else findTargets(graph, tl)

    case Nil => graph.concreteNodes.toStream filter { n => (graph, constraints).wrongUsers(n.id).nonEmpty ||
      (graph, constraints).isWronglyContained(n.id) }
  }


  def nextStates(state : (DependencyGraph, Int)) : Seq[LoggedTry[(DependencyGraph, Int)]] = {
    val (g, violationTarget) = state

    def isViolationTarget(nid: NodeId): Boolean =
      ((g, constraints).isWronglyUsed(nid) || (g, constraints).isWronglyContained(nid))

    //println(s"nextStates $violationTarget")
    def aux(violationTarget : ConcreteNode) : Seq[LoggedTry[(DependencyGraph, Int)]] =
      if(!isViolationTarget(violationTarget.id)) Seq(LoggedSuccess((g, -1)))
      else setState((redirectTowardAbstractions(violationTarget)(g) ++ moveAction(violationTarget)(g) ++ moveContainerAction(violationTarget)(g) ++
            absIntro(violationTarget)(g) ++ hostIntroAction(violationTarget)(g) ++ hostAbsIntro(violationTarget)(g), violationTarget.id))



    if(violationTarget == -1) findTargets(g) flatMap aux
    else aux(g  getConcreteNode violationTarget)
  }
}


trait ActionGenerator {
  val rules: TransformationRules
  val initialGraph: DependencyGraph
  val constraints: ConstraintsMaps

  def initialState: (DependencyGraph, Int) = (initialGraph, -1)


  implicit def setState(s: (Seq[LoggedTry[DependencyGraph]], Int)): Seq[LoggedTry[(DependencyGraph, Int)]] =
    s._1 map (_ map ((_, s._2)))

  val actionsGenerator = new SolvingActions(rules, constraints)

  def extractGraph[A](ng: (A, DependencyGraph)): DependencyGraph = ng._2

  def toSeqLTG[T](s: Seq[LoggedTry[(T, DependencyGraph)]]): Seq[LoggedTry[DependencyGraph]] =
    s map (_ map (_._2))

  val epsilon: DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    g => Seq(LoggedSuccess("Epsilon transition\n", g))


  def hostIntroAction(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.hostIntro(node) andThen toSeqLTG

  def moveAction(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.move(node) andThen toSeqLTG

  def moveContainerAction(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] = {
    dg =>
      val s = dg.container(node.id) map (id => Seq(dg.getConcreteNode(id))) getOrElse Seq()
      toSeqLTG(s flatMap (n => actionsGenerator.move(n)(dg)))
  }

  def redirectTowardAbstractions(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.redirectTowardExistingAbstractions(node)

  def absIntro(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.absIntro(node) andThen toSeqLTG

  def hostAbsIntro(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] ={
    dg =>
      val s = dg.container(node.id) map (id => Seq(dg.getConcreteNode(id))) getOrElse Seq()
      toSeqLTG(s flatMap (n => actionsGenerator.absIntro(n)(dg)))
  }

  //  def checkSuccess(g : DependencyGraph ) : LoggedTry[DependencyGraph] =
  //    if((g, constraints).isWronglyUsed(node.id)) LoggedError("Remaining violations")
  //    else LoggedSuccess(g)

}