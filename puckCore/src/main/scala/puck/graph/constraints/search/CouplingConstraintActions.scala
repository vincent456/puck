package puck.graph
package constraints.search

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations.TransformationRules
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, CreateVarStrategy}
import puck.search._
import puck.util.LoggedEither
import puck.util.LoggedEither._

import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.{-\/, NonEmptyList, \/, \/-}




class CouplingConstraintSolvingAllViolationsControl
(strategy : SearchStrategy[DependencyGraph],
 val rules : TransformationRules,
 val initialGraph : DependencyGraph,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchStrategyDecorator[DependencyGraph](strategy){

  def findTargets(graph : DependencyGraph,
                  l : Seq[NodeKind] = violationsKindPriority.toStream) : Seq[DGNode] =  l match {
    case topPriority +: tl =>
      val tgts = graph.nodes.toStream filter { n =>
        n.kind == topPriority && (graph.wrongUsers(n.id).nonEmpty ||
          graph.isWronglyContained(n.id))
      }
      if(tgts.nonEmpty) tgts
      else findTargets(graph, tl)

    case Seq() => graph.nodes.toStream filter { n => graph.wrongUsers(n.id).nonEmpty ||
      graph.isWronglyContained(n.id) }
  }

  def nextStates(g : DependencyGraph) : Seq[LoggedTry[DependencyGraph]] = {
    val targets = findTargets(g)

    targets flatMap {
      case vn : VirtualNode =>
        Seq(LoggedError[DependencyGraph](s"cannot solve violations toward virtual node $vn"))

      case cn : ConcreteNode =>
        val searchControlStrategy =
          new CouplingConstraintSolvingControl(
            new DepthFirstSearchStrategy[(DependencyGraph, Int)],
            rules, g, cn)

        val engine =
          new SearchEngine(searchControlStrategy.initialState,
            searchControlStrategy,
            Some(1))

        engine.explore()
        engine.successes map (ss => ss.loggedResult map (_._1))
    }
  }

  def initialState : SearchState[DependencyGraph] =
    new SearchState(0, None, LoggedSuccess(initialGraph), nextStates(initialGraph))

  override def oneStep: Option[(LoggedTry[DependencyGraph],
    Seq[LoggedTry[DependencyGraph]])] = {
    strategy.nextState.nextChoice flatMap( lt =>
      lt.value match{
        case \/-(g) => Some((lt, nextStates(g)))
        case -\/(_) => None
      })
  }

}

class CouplingConstraintSolvingControl
(strategy : SearchStrategy[(DependencyGraph, Int)],
 val rules : TransformationRules,
 val initialGraph : DependencyGraph,
 val violationTarget : ConcreteNode
  ) extends SearchStrategyDecorator[(DependencyGraph, Int)](strategy){


  def initialState : SearchState[(DependencyGraph, Int)] =
    new SearchState(0, None, LoggedSuccess((initialGraph, 0)), nextStates(initialGraph,0))

  implicit def setState( s : (Seq[LoggedTry[DependencyGraph]], Int) ) : Seq[LoggedTry[(DependencyGraph, Int)]] =
     s._1 map ( _ map ((_, s._2)))


  def nextStates(g : DependencyGraph, state : Int) : Seq[LoggedTry[(DependencyGraph, Int)]] =
    state match {
      case 0 => (epsilon(g) ++ hostIntroAction(g), 1)

      case 1 => (moveAction(g), 2)

      case 2 => (epsilon(g) ++ absIntro(g), 3)

      case 3 => (redirectTowardAbstractions(g), 4)

      case _ => Seq()

    }

  override def oneStep: Option[(LoggedTry[(DependencyGraph, Int)],
                                Seq[LoggedTry[(DependencyGraph, Int)]])] = {
     strategy.nextState.nextChoice flatMap( lt =>
       lt.value match{
         case \/-((g, state)) => Some((lt, nextStates(g,state)))
         case -\/(_) => None
    })
  }


  val actionsGenerator = new SolvingActions(rules)

  def extractGraph[A](ng : (A, DependencyGraph)): DependencyGraph = ng._2

  val epsilon : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
        g => Seq(LoggedSuccess("Epsilon transition", g))

  val hostIntroAction : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
       actionsGenerator.hostIntro(violationTarget) andThen (_ map ( _ map extractGraph ) )

  val moveAction : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
       actionsGenerator.move(violationTarget) andThen (_ map ( _ map extractGraph))

  val redirectTowardAbstractions : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.redirectTowardExistingAbstractions(violationTarget)

  val absIntro : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
      actionsGenerator.absIntro(violationTarget) andThen (_ map ( _ map extractGraph))



}

class SolvingActions
(val rules : TransformationRules){

  var newCterNumGen = 0

  def containerKind
  ( g : DependencyGraph, toBeContainedKind : NodeKind
    ) : Stream[NodeKind] = {
    g.nodeKinds.toStream.filter(_.canContain(toBeContainedKind))
  }

  def hostIntro
  (toBeContained : ConcreteNode
    ) : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    g =>
      containerKind(g, toBeContained.kind) map {
        hostKind =>
          newCterNumGen += 1
          val hostName = s"${toBeContained.name}_container$newCterNumGen"
          rules.intro(g, hostName, hostKind)
      } flatMap {
        case (n, g1) =>
          findHost(n)(g1) map (ltg => s"Searching host for $n\n" <++: ltg map {
            case (hid, g2) => (n.id, g2.addContains(hid, n.id))
          })
      }

  def findHost
  ( toBeContained : ConcreteNode ) : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    chooseNode((dg, cn) => dg.canContain(cn, toBeContained))

  type NodePredicate = (DependencyGraph, ConcreteNode) => Boolean

  def canBeVirtualized : KindType => Boolean = {
    case NameSpace => true
    case _ => false
  }

  def logNodeChosen(n : DGNode, graph : DependencyGraph) : LoggedTry[(NodeId, DependencyGraph)] =
    LoggedSuccess(s"node chosen is $n\n", (n.id, graph))


  def chooseNode
  ( predicate : NodePredicate)
  : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
    graph =>

      val choices = graph.concreteNodes.filter(predicate(graph,_)).toList

      if(choices.isEmpty) Stream(LoggedError("choose node, no choice"))
      else {

        val s = partitionByKind(graph)(choices, List()).toStream


        s flatMap {
          nel =>
            if (nel.tail.isEmpty) Stream(logNodeChosen(nel.head, graph))
            else if (canBeVirtualized(nel.head.kind.kindType)) {
              val (vn, g2) = graph.addVirtualNode(nel.toList.map(_.id).toSeq, nel.head.kind)
              Stream(logNodeChosen(vn, g2))
            }
            else {
              nel.toStream map (logNodeChosen(_, graph))
            }
        }
      }
 }

  def partitionByKind
  ( graph : DependencyGraph
    ) : (List[DGNode], List[NonEmptyList[DGNode]]) => List[NonEmptyList[DGNode]] = {
    case (Nil, acc) => acc
    case (hd :: tl, acc) =>
      val (same, diff) = tl.partition(n => n.kind == hd.kind)
      partitionByKind(graph)(diff, NonEmptyList[DGNode](hd, same:_*) +: acc)
  }



  def createVarStrategies
  ( g: DependencyGraph ): Stream[CreateVarStrategy] = {
    val tmKinds = g.nodeKindKnowledge.kindOfKindType(InstanceValueDecl)
    (CreateParameter +: (tmKinds map CreateTypeMember.apply)).toStream
  }


  def move
  ( wronglyContained : ConcreteNode
    ) : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
    g0 =>
      findHost(wronglyContained)(g0) flatMap {
        case LoggedEither(log, -\/(err)) => Stream(LoggedEither(log, -\/(err)))
        case LoggedEither(log, \/-((newCter, g))) =>
              val oldCter = g.container_!(wronglyContained.id)
              doMove(g, wronglyContained, oldCter, newCter) map (ltg => ltg.map((newCter, _)))
      }
  }


  def doMove
    ( g: DependencyGraph,
      wronglyContained : ConcreteNode,
      oldCter : NodeId,
      newCter : NodeId
    ) :  Stream[LoggedTG] = {
    val stream: Stream[LoggedTG] = (wronglyContained.kind.kindType, g.styp(wronglyContained.id)) match {
      case (InstanceValueDecl, Some(typ)) =>

        val needNewReceiver = !(typ uses newCter)

        val isPullUp = g.isa_*(oldCter, newCter)
        val isPushDown = g.isa_*(newCter, oldCter)

        (isPullUp, isPushDown) match {
          case (true, true) =>
            assert(oldCter == newCter)
            Stream(LoggedSuccess(g))
          case (true, _) =>
            Stream(rules.move.pullUp(g, List(wronglyContained.id), oldCter, newCter))
          case (_, true) =>
            Stream(rules.move.pushDown(g, List(wronglyContained.id), oldCter, newCter))
          case _ if needNewReceiver =>
            createVarStrategies(g) map {
              cvs =>
                rules.move.typeMemberBetweenUnrelatedTypeDecl(g,
                  List(wronglyContained.id), oldCter, newCter, Some(cvs))
            }
          case _ =>
            Stream(rules.move.typeMemberBetweenUnrelatedTypeDecl(g, List(wronglyContained.id), oldCter, newCter, None))
        }

      case (TypeDecl, _) =>
        Stream(rules.move.staticDecl(g, wronglyContained.id, newCter))

      case _ => ???
    }

    stream map { ltg => s"Moving $wronglyContained " +
      s"from ${(g, oldCter).shows} to ${(g, newCter).shows}" <++: ltg }
  }

  def absIntro
  (impl : ConcreteNode
  ) : DependencyGraph => Stream[LoggedTry[(Abstraction, DependencyGraph)]] =
    g =>
      impl.kind.abstractionChoices.toStream map {
        case (absNodeKind, absPolicy) =>
          rules.abstracter.createAbstraction(g, impl, absNodeKind, absPolicy)
      } flatMap {
        case LoggedEither(log, -\/(err)) => Stream(LoggedEither(log, -\/(err)))
        case LoggedEither(log, \/-((abs, g2))) =>
          val absNodeKind = abs.kind(g2)

          (hostIntro(g2.getConcreteNode(abs.nodes.head))(g2) ++
            chooseNode(rules.abstracter.absIntroPredicate(impl,
              abs.policy, absNodeKind))(g2)).map {
                lt => lt.flatMap {
                  case (host, g3) =>
                    s"Searching host for $abs\n" <++: introAbsContainsAndIsa(abs, impl, g3, host)
                }
          }
      }



/*  def absIntro
  (impl : ConcreteNode
    ) : DependencyGraph => Stream[LoggedTry[(Abstraction, DependencyGraph)]] =
    g =>
    impl.kind.abstractionChoices.toStream flatMap {
      case (absNodeKind, absPolicy) =>

        chooseNode(rules.abstracter.absIntroPredicate(impl,
          absPolicy, absNodeKind))(g) .map { lt =>
           lt.flatMap {
            case (host, g2) =>
              rules.abstracter.createAbstraction(g2, impl, absNodeKind, absPolicy).flatMap {
                case (abs, g3) =>
                  s"Searching host for $abs\n" <++: introAbsContainsAndIsa(abs, impl, g3, host)
              }
          }
        }

    }*/

  def introAbsContainsAndIsa
   ( abs : Abstraction,
     impl : ConcreteNode,
     g : DependencyGraph,
     host : NodeId) : LoggedTry[(Abstraction, DependencyGraph)] = {
    val g2 = abs.nodes.foldLeft(g){_.addContains(host, _)}

    (g2.container(impl.id), g2.kindType(host)) match {
      case (None, _) => LoggedError("current impl has no container")
      case (Some(c), TypeDecl) =>
        val graph5 = g2.addAbstraction(c, AccessAbstraction(host, abs.policy))
        val graph6 =
          if(abs.policy == SupertypeAbstraction)
            graph5.addIsa(c, host).addUses(c, host)
          else graph5
        LoggedSuccess((abs, graph6))

      case _ => LoggedSuccess((abs, g2))
    }
    
  }


  def redirectTowardExistingAbstractions
  ( used : ConcreteNode) :
    DependencyGraph => Stream[LoggedTG] =
    g => {
      redirectTowardExistingAbstractions(used,
        g wrongUsers used.id,
        g abstractions used.id)(g)
    }



  def redirectTowardExistingAbstractions
  ( used : ConcreteNode,
    wrongUsers : List[NodeId],
    choices : Set[Abstraction]) :
    DependencyGraph => Stream[LoggedTG] =
    g =>
      if(wrongUsers.isEmpty) Stream(LoggedSuccess(g))
      else {

      val cannotUseAbstraction: Abstraction => NodeId => Boolean = {
        abs => userId =>

          val uses = g.getUsesEdge(userId, used.id).get

          (abs, uses.accessKind) match {
            case (AccessAbstraction(absId, _), _) => g.interloperOf(userId, absId)
            case (ReadWriteAbstraction(Some(rid), _), Some(Read)) => g.interloperOf(userId, rid)
            case (ReadWriteAbstraction(_, Some(wid)), Some(Write)) => g.interloperOf(userId, wid)
            case (ReadWriteAbstraction(Some(rid), Some(wid)), Some(RW)) =>
              g.interloperOf(userId, rid) || g.interloperOf(userId, wid)
            case _ => sys.error("should not happen")
          }

      }

      choices.toStream flatMap {
          abs =>
             val (remainingWus, wuToRedirect) =
                wrongUsers.partition(cannotUseAbstraction(abs))

              val ltg: LoggedTG =
                wuToRedirect.foldLoggedEither(g) {
                  (g, wu) =>
                    rules.redirection.
                      redirectUsesAndPropagate(g, g.getUsesEdge(wu, used.id).get, abs)
                }

            ltg.value match {
              case \/-(g2) =>
                redirectTowardExistingAbstractions(used,remainingWus, choices - abs)(g2)
              case -\/(_) => Stream(ltg)
            }
          }
      }





 /* implicit val LTM = puck.util.LoggedEither.loggedEitherMonad[Error]

  type FindHost = (DependencyGraph, ConcreteNode) => LoggedTry[(NodeId, DependencyGraph)]
  type DoMove = (DependencyGraph, ConcreteNode, NodeId, NodeId) => (LoggedTG \/ (CreateVarStrategy => LoggedTG))

  val solveContains0 :
  (DependencyGraph, ConcreteNode) =>
    FindHost => DoMove => (LoggedTG \/ LoggedTry[(CreateVarStrategy => LoggedTG)]) =

    ( g, wronglyContained )  => findHost => doMove => {

    val oldCter = g.container_!(wronglyContained.id)

    val g0 = g.removeContains(oldCter, wronglyContained.id, register = false)

    def checkContainsSolved(g : DependencyGraph) : LoggedTG =
      if(g.isWronglyContained(wronglyContained.id)) LoggedSuccess(g)
      else LoggedError("constraint unsolvable")


    /*findHost(g0, wronglyContained,
      (graph : DependencyGraph, potentialHost: ConcreteNode) =>
        !graph.interloperOf(potentialHost.id, wronglyContained.id))*/

    val moveThenCheck: (NodeId, DependencyGraph) => (LoggedTG \/ (CreateVarStrategy => LoggedTG)) =
      (newCter, g1) =>
        doMove(g1.addContains(oldCter, wronglyContained.id, register = false),
          wronglyContained, oldCter, newCter).bimap(
          _ flatMap checkContainsSolved,
          f => f andThen (_ flatMap checkContainsSolved)
        )

      LTM.cozip(findHost(g0, wronglyContained) map moveThenCheck.tupled).bimap(LTM.join,identity)

  }*/


}

