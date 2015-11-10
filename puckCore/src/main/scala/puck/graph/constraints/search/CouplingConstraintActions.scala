package puck.graph.constraints.search

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations.TransformationRules
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, CreateVarStrategy}
import puck.search.{SearchState, SearchStrategy}
import puck.util.LoggedEither._

import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.{-\/, NonEmptyList, \/, \/-}

class SearchStrategyDecorator[T]
(val strategy : SearchStrategy[T])
  extends SearchStrategy[T] {
  override def addState(s: SearchState[T]): Unit =
    strategy.addState(s)

  override def addState(currentResult: LoggedTry[T],
                        choices: Seq[LoggedTry[T]]): Unit =
    strategy.addState(currentResult, choices)

  override def nextState: SearchState[T] = strategy.nextState

  override def canContinue: Mutability = strategy.canContinue

  override def currentState: SearchState[T] = strategy.currentState

  override def oneStep: Option[(LoggedTry[T], Seq[LoggedTry[T]])] = strategy.oneStep
}


class CouplingConstraintSolvingControl
(strategy : SearchStrategy[(DependencyGraph, Int)],
 val rules : TransformationRules,
 val initialGraph : DependencyGraph,
 val violationTarget : ConcreteNode
  ) extends SearchStrategyDecorator[(DependencyGraph, Int)](strategy){


  def initialState : SearchState[(DependencyGraph, Int)] =
    new SearchState(0, None, LoggedSuccess((initialGraph,0)), nextStates(initialGraph,0))

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
       actionsGenerator.hostIntro(violationTarget) andThen (_ map ( ng => LoggedSuccess(ng._2) ) )

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
  ( g : DependencyGraph, toBeContained : DGNode
    ) : Stream[NodeKind] = {
    g.nodeKinds.toStream.filter(_.canContain(toBeContained.kind))
  }

  def hostIntro
  (toBeContained : ConcreteNode
    ) : DependencyGraph => Stream[(NodeId, DependencyGraph)] =
    g =>
      containerKind(g, toBeContained) map {
        hostKind =>
          newCterNumGen += 1
          val hostName = s"${toBeContained.name}_container$newCterNumGen"
          rules.intro(g, hostName, hostKind)
      } flatMap {
        case (n, g1) =>
          findHost(n)(g1)
      }

  def findHost
  ( toBeContained : ConcreteNode
    ) : DependencyGraph => Stream[(NodeId, DependencyGraph)] =
    chooseNode(_.canContain(_, toBeContained))

  type NodePredicate = (DependencyGraph, ConcreteNode) => Boolean

  def canBeVirtualized : KindType => Boolean = {
    case NameSpace => true
    case _ => false
  }

  def chooseNode
  ( predicate : NodePredicate)
  : DependencyGraph => Stream[(NodeId, DependencyGraph)] = {
    graph =>

      val choices = graph.concreteNodes.filter(predicate(graph,_)).toList

      val s = partitionByKind(graph)(choices, List()).toStream

      s flatMap {
        nel =>
          if(nel.tail.isEmpty) Stream( (nel.head.id, graph) )
          else if (canBeVirtualized (nel.head.kind.kindType)){
            val (vn, g2) = graph.addVirtualNode(nel.toList.map(_.id).toSeq,  nel.head.kind)
            Stream( (vn.id, g2) )
          }
          else {
            nel.toStream map (n => (n.id, graph))
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
    ) : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    g0 =>
      findHost(wronglyContained)(g0) flatMap {
        case (newCter, g) =>
          val oldCter = g.container_!(wronglyContained.id)

          doMove(g, wronglyContained, oldCter, newCter) map ( ltg => ltg.map((newCter,_)))
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
    impl.kind.abstractionChoices.toStream flatMap {
      case (absNodeKind, absPolicy) =>
        chooseNode(rules.abstracter.absIntroPredicate(impl,
          absPolicy, absNodeKind.kindType))(g).map {
          case (host, g2) =>
            rules.abstracter.createAbstraction(g2, impl, absNodeKind, absPolicy).flatMap {
              case (abs, g3) => introAbsContainsAndIsa(abs, impl, g3, host)
                
            }
        }

    }

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





  implicit val LTM = puck.util.LoggedEither.loggedEitherMonad[Error]

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

  }


}

