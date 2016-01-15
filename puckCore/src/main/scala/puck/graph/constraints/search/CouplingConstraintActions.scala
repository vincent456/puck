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
(val rules : TransformationRules,
 val initialGraph : DependencyGraph,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[DependencyGraph] {

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


        val engine =
          new SearchEngine(new DepthFirstSearchStrategy(),
            new CouplingConstraintSolvingControl(rules, g, cn),
            Some(1))

        engine.explore()
        engine.successes map (ss => ss.loggedResult map (_._1))
    }
  }

  def initialState : SearchState[DependencyGraph] =
    new SearchState(0, None, LoggedSuccess(initialGraph), nextStates(initialGraph))

}

class CouplingConstraintSolvingControl
(val rules : TransformationRules,
 val initialGraph : DependencyGraph,
 val violationTarget : ConcreteNode
  ) extends SearchControl[(DependencyGraph, Int)]{


  def initialState : SearchState[(DependencyGraph, Int)] =
    new SearchState(0, None, LoggedSuccess((initialGraph, 0)), nextStates((initialGraph,0)))



  implicit def setState( s : (Seq[LoggedTry[DependencyGraph]], Int) ) : Seq[LoggedTry[(DependencyGraph, Int)]] =
     s._1 map ( _ map ((_, s._2)))
//  implicit def setState2[T]( s : (Seq[LoggedTry[(T, DependencyGraph)]], Int) ) : Seq[LoggedTry[(DependencyGraph, Int)]] =
//    s._1 map ( _ map {case (t,g) =>  (g, s._2)})


  def nextStates(state : (DependencyGraph, Int)) : Seq[LoggedTry[(DependencyGraph, Int)]] = {
    val  (g, automataState) = state
    if(g.isWronglyUsed(violationTarget.id) || g.isWronglyContained(violationTarget.id))
      automataState match {
        case 0 =>
          val s =
            setState((epsilon(g) ++ hostIntroAction(g), 1)) ++
              setState((hostAbsIntro(g), 3)) ++
              setState((moveContainerAction(g), 4))
          assert(s.nonEmpty)
          s

        case 1 => val s = (moveAction(g), 2)
          assert(s.nonEmpty)
          s
        case 2 => val s = (epsilon(g) ++ absIntro(g), 3)
          assert(s.nonEmpty)
          s
        case 3 => val s = (redirectTowardAbstractions(g), 4)
          assert(s.nonEmpty)
          s
        case _ => Seq()
      }
    else Seq()
  }


  val actionsGenerator = new SolvingActions(rules)

  def extractGraph[A](ng : (A, DependencyGraph)): DependencyGraph = ng._2

  def toSeqLTG[T]( s : Seq[LoggedTry[(T, DependencyGraph)]] ) :  Seq[LoggedTry[DependencyGraph]] =
    s map ( _ map (_._2))

  val epsilon : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
        g => Seq(LoggedSuccess("Epsilon transition\n", g))

//  val nameSpaceIntro: DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
//    g => toSeqLTG(actionsGenerator.introNodes(g.nodeKindKnowledge.kindOfKindType(NameSpace), g))
//
//  val typeDeclIntro: DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
//    g => toSeqLTG(actionsGenerator.introNodes(g.nodeKindKnowledge.kindOfKindType(TypeDecl), g))

  val hostIntroAction : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
       actionsGenerator.hostIntro(violationTarget) andThen toSeqLTG

  val moveAction : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
       actionsGenerator.move(violationTarget) andThen toSeqLTG

  val moveContainerAction : DependencyGraph => Seq[LoggedTry[DependencyGraph]] = {
    dg =>
      val s = dg.container(violationTarget.id) map (id => Seq(dg.getConcreteNode(id))) getOrElse Seq()
      toSeqLTG(s flatMap (n => actionsGenerator.move(n)(dg)))
  }

  val redirectTowardAbstractions : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.redirectTowardExistingAbstractions(violationTarget)

  val absIntro : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
      actionsGenerator.absIntro(violationTarget) andThen toSeqLTG

  val hostAbsIntro : DependencyGraph => Seq[LoggedTry[DependencyGraph]] ={
    dg =>
      val s = dg.container(violationTarget.id) map (id => Seq(dg.getConcreteNode(id))) getOrElse Seq()
      toSeqLTG(s flatMap (n => actionsGenerator.absIntro(n)(dg)))
  }

  def checkSuccess(g : DependencyGraph ) : LoggedTry[DependencyGraph] =
    if(g.isWronglyUsed(violationTarget.id)) LoggedError("Remaining violations")
    else LoggedSuccess(g)

}

class SolvingActions
(val rules : TransformationRules) {

  var newCterNumGen = 0

  def containerKind
  (g : DependencyGraph,
   toBeContainedKind: NodeKind
  ): Stream[NodeKind] =
    g.nodeKinds.toStream.filter(_.canContain(toBeContainedKind))



  /*  def hostIntro
  (toBeContained : ConcreteNode
  ) : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    g => hostIntro(containerKind(g, toBeContained.kind))(g)


  def hostIntro
  (containerKinds : Seq[NodeKind]
  ) : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] = { g =>
    println("host intro")
    containerKinds map {
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
  }*/


  def introNodes(nodeKinds: Seq[NodeKind], g: DependencyGraph): Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    nodeKinds.toStream map {
      hostKind =>
        newCterNumGen += 1
        val hostName = s"extra${hostKind}Container$newCterNumGen"
        rules.intro(g, hostName, hostKind)
    } flatMap attribHost


  def hostIntro
  (toBeContained: ConcreteNode
  ): DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    g => containerKind(g, toBeContained.kind) map {
      hostKind =>
        newCterNumGen += 1
        val hostName = s"${toBeContained.name}_container$newCterNumGen"
        rules.intro(g, hostName, hostKind)
    } flatMap {
      case (toBeCtedHost, g) =>
        val hostStream =
        chooseNode((dg, hostHost) => dg.canContain(hostHost, toBeCtedHost) &&
            !dg.isViolation(Contains(hostHost.id, toBeCtedHost.id)) && {
            val dg2 = dg.addContains(hostHost.id, toBeCtedHost.id)
                        .addContains(toBeCtedHost.id, toBeContained.id)
            !dg2.isWronglyUsed(toBeContained.id)
        })(g)

        hostStream  map (ltg => s"Searching host for $toBeCtedHost\n" <++: ltg map {
          case (hid, g1) => (toBeCtedHost.id, g1.addContains(hid, toBeCtedHost.id))
        })
    }


  def findHost
  (toBeContained: ConcreteNode): DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
   chooseNode((dg, cn) => dg.canContain(cn, toBeContained) &&
      !dg.isViolation(Contains(cn.id, toBeContained.id)) && {
      val dg1 : DependencyGraph =
        dg.container(toBeContained.id) map (dg.removeContains(_, toBeContained.id)) getOrElse dg
      val dg2 = dg1.addContains(cn.id, toBeContained.id)
      dg2.subTree(toBeContained.id).forall(!dg2.isWronglyUsed(_)) //needed when moving violation host
    }
   )


  val attribHost : ((ConcreteNode, DependencyGraph)) => Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
    case (n, g) =>
      val hostStream = findHost(n)(g)
      hostStream  map (ltg => s"Searching host for $n\n" <++: ltg map {
      case (hid, g1) => (n.id, g1.addContains(hid, n.id))
    })
  }


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

      if(choices.isEmpty) Stream(LoggedError(s"choose node, no choice"))
      else {

        val s = partitionByKind(graph)(choices, List()).toStream


        s flatMap {
          nel =>
            if (nel.tail.isEmpty) Stream(logNodeChosen(nel.head, graph))
            else if (canBeVirtualized(nel.head.kind.kindType)) {
              val (vn, g2) = graph.addVirtualNode(nel.map(_.id).toSet, nel.head.kind)
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
      val lg = s"trying to move $wronglyContained, searching host\n"
      findHost(wronglyContained)(g0) flatMap {
        case LoggedEither(log, -\/(err)) => Stream(LoggedEither(lg + log, -\/(err)))
        case LoggedEither(log, \/-((newCter, g))) =>
              val oldCter = g.container_!(wronglyContained.id)
              doMove(g, wronglyContained, oldCter, newCter) map (ltg => (lg + log) <++: ltg.map((newCter, _)))
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

      case (TypeDecl, _)
        | (NameSpace, _)
        | (StaticValueDecl, _) =>
        Stream(rules.move.staticDecl(g, wronglyContained.id, newCter))

      case (TypeConstructor, _) =>
        Stream(LoggedError("TypeConstructor cannot be moved"))
      //true for java or C++ but not necessarily for all kind of languages
      //fail in move rule ? (which would rely on a canMove language specific predicate ?)

      case (kindType, styp) => error(s"doMove $kindType typed $styp not expected")
    }

    stream map { ltg => s"Moving $wronglyContained " +
      s"from ${(g, oldCter).shows} to ${(g, newCter).shows}\n" <++: ltg }
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
    g =>
        redirectTowardExistingAbstractions(used,
          g wrongUsers used.id,
          g abstractions used.id)(g)





  def redirectTowardExistingAbstractions
  ( used : ConcreteNode,
    wrongUsers : List[NodeId],
    choices : Set[Abstraction]) :
  DependencyGraph => Stream[LoggedTG] =
    g =>
      if(wrongUsers.isEmpty) Stream(LoggedSuccess(g))
      else if(choices.isEmpty) Stream(LoggedError("No abstractions"))
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
                  // Uses(wu, used.id) can have be deleted in a
                  // previous iteration of this foldLoggedEither loop
                  g.getUsesEdge(wu, used.id) match {
                    case Some(uses) =>
                      rules.redirection.redirectUsesAndPropagate(g, uses, abs)
                    case None => LoggedSuccess(g)
                  }
              }

            ltg.value match {
              case \/-(g2) =>
                redirectTowardExistingAbstractions(used, remainingWus, choices - abs)(g2) map ( ltg.log <++: _)
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

