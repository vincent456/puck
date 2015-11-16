package puck.graph
package constraints


import puck.PuckError
import puck.graph.transformations.TransformationRules
import puck.util.{LoggedEither, Logged}
import puck.util.LoggedEither._
import scalaz.{\/-, -\/}
import scalaz.syntax.writer._
import scalaz.std.string._
import scalaz.std.list._

import ShowDG._

object FindHostResult {
  def host(host : NodeId, graph : DependencyGraph): FindHostResult =
    Host(host, graph)
  def error : FindHostResult = FindHostError
}
trait FindHostResult
case class Host(host : NodeId, graph : DependencyGraph) extends FindHostResult
case object FindHostError extends DGError with FindHostResult

object Solver {
  type IntroKArgs = LoggedTry[(Abstraction, DependencyGraph)]
  type IntroK = IntroKArgs => Unit
}

import Solver._
class Solver
( val decisionMaker : DecisionMaker,
  val rules : TransformationRules,
  val automaticConstraintLoosening : Boolean){

  def redirectTowardExistingAbstractions
  ( lg : LoggedG,
    used : ConcreteNode,
    wrongUsers : List[NodeId])
  ( k : LoggedTry[(DependencyGraph, List[NodeId])] => Unit) : Unit = {

    def aux(lg : LoggedG,
            wrongUsers : List[NodeId],
            choices : Set[Abstraction]
             ) : Unit = {
      val g = lg.value

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

      decisionMaker.selectExistingAbstraction(lg, choices) {
        loggedOption =>
          val log = loggedOption.written
          loggedOption.value match {
            case None =>
              k((g,wrongUsers).set(log).toLoggedTry)
            case Some(abs) =>

              val (remainingWus, wuToRedirect) =
                wrongUsers.partition(cannotUseAbstraction(abs))

              val lg1 = lg :++> s"$wuToRedirect will use abstraction $abs\n"

              val ltg: LoggedTG =
                wuToRedirect.foldLoggedEither(lg1) {
                  (g, wu) =>
                    rules.redirection.
                      redirectUsesAndPropagate(g, g.getUsesEdge(wu, used.id).get, abs)
                }

              ltg.value match {
                case \/-(g2) =>
                  aux(g2.set(ltg.log), remainingWus, choices - abs)
                case -\/(err) =>
                  k(LoggedError(ltg.log, err))
              }
          }
      }

    }
    aux( lg :++> "redirect toward existing abstractions\n",
          wrongUsers, lg.value.abstractions(used.id))
  }



  var newCterNumGen = 0

  private val allwaysTrue : NodePredicate = (_,_) => true


  def hostIntro
    (lg : LoggedG,
     toBeContained : ConcreteNode,
     parentsThatCanBeCreated : Int,
     k : Logged[FindHostResult] => Unit) : Unit =
    decisionMaker.chooseContainerKind(lg :++> s"call hostIntro $toBeContained\n", toBeContained){
      loggedSkind =>

        val log = loggedSkind.written
        loggedSkind.value match {
          case None =>
            val log1 =
              log + s"do not know how to create a valid host for ${toBeContained.kind}\n"
            k(FindHostResult.error.set(log1))
          case Some(hostKind) =>
            newCterNumGen += 1
            val hostName = s"${toBeContained.name}_container$newCterNumGen"
            val (host, g1) = rules.intro(lg.value, hostName, hostKind)
            val log1 =
              log + s"creating $hostName host intro, rec call to find host " +
                s"($parentsThatCanBeCreated parents can be created)\n"

            findHost(g1.set(log1),
              host, allwaysTrue, parentsThatCanBeCreated - 1) {
              loggedRes =>
                loggedRes.value match {
                  case Host(hostsHostId, graph3) =>
                    k(loggedRes.map(_ =>
                      Host(host.id, graph3.addContains(hostsHostId, host.id))))
                  case FindHostError =>
                    k(loggedRes :++> "host intro transmit find host error\n")
                }

            }
        }
    }



  def findHost(lg : LoggedG,
               toBeContained : ConcreteNode,
               specificPredicate : NodePredicate = allwaysTrue,
               parentsThatCanBeCreated : Int = 1)
              (k : Logged[FindHostResult] => Unit) : Unit = {

    object FindHostPredicate extends NodePredicate {
      def apply(graph : DependencyGraph, n : ConcreteNode) : Boolean =
        graph.canContain(n, toBeContained) && specificPredicate(graph, n)

      override def toString : String = s"Searching Host for $toBeContained"
    }

    val lg1 = lg :++>
      s"findHost(_, $toBeContained, _, parentsThatCanBeCreated = $parentsThatCanBeCreated)\n"

    decisionMaker.chooseNode(lg1, FindHostPredicate){
      loggedGraphAndSomeNodeId =>
        val log = loggedGraphAndSomeNodeId.written
        loggedGraphAndSomeNodeId.value match {
          case None =>
            if(parentsThatCanBeCreated == 0)
              k(FindHostResult.error.
                set( log + "host intro, ancestor's max limit is not enough\n"))
            else
              hostIntro(lg1.value.set( log + "find host, no node given by decision maker : call to host intro\n"),
                toBeContained, parentsThatCanBeCreated, k)
        case (Some((g, nid))) =>
          k(FindHostResult.host(nid, g).
            set(log + s"find host: decision maker chose ${(g, nid).shows} to contain $toBeContained\n"))
      }
    }
  }



  def findHostAfterAbsIntro
  ( currentImplId : NodeId, //TODO look TODO in body to remove this arg
    abs : Abstraction,
    k : LoggedTG => Unit,
    pred : NodePredicate,
    lg : LoggedG) : Unit = {

    val abstractionPolicy : AbstractionPolicy = abs.policy

    val lg1 = lg :++> s"Searching host for $abs\n"

    findHost(lg1, lg.value.getConcreteNode(abs.nodes.head), pred) {
      logres =>
        logres.value match {
          case Host(h, graph3) =>
            val log = logres.written +
              s"absIntro : host of $abs is ${(graph3, h).shows}\n"

            val graph4 = abs.nodes.foldLeft(graph3){_.addContains(h, _)}

            (graph4.container(currentImplId), graph4.kindType(h)) match {
              case (None, _) => k(LoggedError("current impl has no container"))
              case (Some(c), TypeDecl) =>
                 val graph5 = graph4.addAbstraction(c, AccessAbstraction(h, abs.policy))
                 val graph6 =
                      if(abs.policy == SupertypeAbstraction)
                        graph5.addIsa(c, h).addUses(c, h)
                      else graph5
                k(LoggedSuccess(graph6))

              case _ => k(LoggedSuccess(graph4))
            }

          case FindHostError =>
            k(LoggedError(logres.written + "error while searching host for abstraction",
            FindHostError))
        }
    }

    def aux( remainingThatNeedHost : List[ConcreteNode] ) : Unit = {
      remainingThatNeedHost match {
        case Nil => k(lg.toLoggedTry)
        case absNId :: tl =>



      }
    }

    aux(abs.nodes map lg.value.getConcreteNode)

  }



  def absIntro(lg : LoggedG,
               impl : ConcreteNode,
               wrongUsers : List[NodeId],
               degree : Int = 1)
               (k : LoggedTry[DependencyGraph] => Unit) : Unit = {

    def aux(lg : LoggedG, deg : Int, currentImpl : ConcreteNode)
           (k : IntroK ) : Unit = {
      val lg1 = lg :++> s"*** abs intro degree $deg/$degree ***\n"
      decisionMaker.abstractionKindAndPolicy(lg1, currentImpl){
        loggedSKindPolicy =>
          loggedSKindPolicy.value match {
          case Some((absNodeKind, absPolicy)) =>

            def doIntro(k: IntroK): Unit = {
              val graph = lg1.value
              val log = loggedSKindPolicy.written +
                s"trying to create abstraction( $absNodeKind, $absPolicy ) of $currentImpl\n"


              rules.abstracter.createAbstraction(graph, currentImpl, absNodeKind, absPolicy) map {
                case (abs, graph2) =>
                  findHostAfterAbsIntro(currentImpl.id, abs, ltg => k(ltg.map((abs, _))),
                    rules.abstracter.absIntroPredicate(currentImpl, absPolicy, absNodeKind),
                    graph2.set(log))
              }
              ()
            }

            if (deg == degree) doIntro(k)
            else
              doIntro {
                ltg  =>
                  ltg.value match {
                    case -\/(_) =>
                      k(ltg :++> s"Single abs intro degree $deg/$degree error (currentImpl = $currentImpl)")
                    case \/-((AccessAbstraction(absId, _), g)) =>
                      aux(g.set(ltg.log), deg + 1, g.getConcreteNode(absId))(k)
                    case \/-((rwAbs @ ReadWriteAbstraction(_,_), g)) => ???
//                      rwAbs.toList.map(g.getConcreteNode)
//                    aux(g.set(ltg.log), deg + 1, g.getConcreteNode(absId))(k)
                  }
              }

          case None =>
            k(LoggedError(loggedSKindPolicy.written,
              new DGError(s"no abstraction for impl of kind $currentImpl")))
        }
      }
    }


    def redirectWrongUsers(lgt : IntroKArgs ) : Unit = {

      k(lgt.flatMap {
        case (abs, g) =>
          val lg = g.set("redirecting wrong users !!")
          wrongUsers.foldLoggedEither[PuckError, DependencyGraph](lg) {
            (g, wuId) =>
              rules.redirection.redirectUsesAndPropagate(g,
                Uses(wuId, impl.id), abs)
          }
      })
    }

    aux (lg :++> s"abs of $impl intro degree $degree", 1, impl) (redirectWrongUsers)
  }

  def solveUsesToward
    ( lg : LoggedG,
      impl : ConcreteNode,
      k : LoggedTry[DependencyGraph] => Unit) : Unit = {

    val lg1 = lg :++>
            ("###################################################\n" +
              s"##### Solving uses violations toward $impl ######\n")

    redirectTowardExistingAbstractions(lg1, impl, lg.value.wrongUsers(impl.id)){
      loggedTGraphWusers : LoggedTry[(DependencyGraph, List[NodeId])] =>

        loggedTGraphWusers.value match {
          case \/-((graph2, wrongUsers)) =>
            if (wrongUsers.nonEmpty)
              absIntro(graph2.set(loggedTGraphWusers.log), impl, wrongUsers)(k)
            else k(loggedTGraphWusers map (_._1))
          case -\/(err) =>
            k(LoggedError(loggedTGraphWusers.log, err))
        }


    }
  }

  def solveContains
  ( lg: LoggedG,
    wronglyContained : ConcreteNode,
    k0 : LoggedTG => Unit) : Unit = {

    val oldCter = lg.value.container(wronglyContained.id).get
    val lg1 = (lg :++> ("###################################################\n" +
              s"##### Solving contains violations toward $wronglyContained ######\n"))
      // detach for host searching : do not want to consider parent constraints
      .map(_.removeContains(oldCter, wronglyContained.id, register = false))

    findHost(lg1, wronglyContained,
      (graph : DependencyGraph, potentialHost: ConcreteNode) =>
        !graph.interloperOf(potentialHost.id, wronglyContained.id)) {
      logres =>
        logres.value match {
          case Host(newCter, g) =>

            val log =
                logres.written +
                  s"solveContains : host of $wronglyContained will now be ${(g, newCter).shows}\n"

            def checkIfMoveSolveContains( tg : LoggedTG) : LoggedTG =
              tg.flatMap(g =>
                (g.isWronglyContained(wronglyContained.id), automaticConstraintLoosening) match {
                  case (false, _) => LoggedSuccess(g)
                  case (true, true) => LoggedSuccess(rules.addHideFromRootException (g, wronglyContained.id, newCter))
                  case (true, false) => LoggedError("constraint unsolvable")
                })

            def k : (String, LoggedTG) => Unit = {
              (log, ltg) =>
                k0(checkIfMoveSolveContains(log <++: ltg))
              //k0 compose checkIfMoveSolveContains
            }
            //checkIfMoveSolveContains andThen k0
            //re-attach before moving

            val g2 = g.addContains(oldCter, wronglyContained.id, register = false)


            (wronglyContained.kind.kindType, g.styp(wronglyContained.id)) match {
              case (InstanceValueDecl, Some(typ)) =>

                val needNewReceiver = !(typ uses newCter)

                val isPullUp = g.isa_*(oldCter, newCter)
                val isPushDown = g.isa_*(newCter, oldCter)

                (isPullUp, isPushDown) match {
                  case (true, true) =>
                    assert(oldCter == newCter)
                    k(log, LoggedSuccess(g2))
                  case (true, _) =>
                    k(log, rules.move.pullUp(g2, List(wronglyContained.id), oldCter, newCter))
                  case (_, true) =>
                    k(log, rules.move.pushDown(g2, List(wronglyContained.id), oldCter, newCter))
                  case _ if needNewReceiver =>
                    decisionMaker.createVarStrategy(g2.set("")) {
                      cvs =>
                        val ltg =
                          rules.move.typeMemberBetweenUnrelatedTypeDecl(g2, List(wronglyContained.id), oldCter, newCter, Some(cvs.value))
                        k(log + cvs.written, ltg)
                    }
                  case _ =>
                    k(log, rules.move.typeMemberBetweenUnrelatedTypeDecl(g2, List(wronglyContained.id), oldCter, newCter, None))
                }

              case (TypeDecl,_) =>
                k(log, rules.move.staticDecl (g2, wronglyContained.id, newCter))

              case _ => ???
            }

          case FindHostError =>
            k0(LoggedError(logres.written, new DGError("FindHostError caught")))
        }
    }
  }


  def solveViolationsToward
  ( lg : LoggedG ,
    target : ConcreteNode)
  ( k: LoggedTG => Unit ) = {
    def end: LoggedTG => Unit = {
      ltg =>
        ltg.value match {
        case \/-(g) =>
          val log = ltg.log + s"solveViolationsToward $target end"
          if (g.wrongUsers(target.id).nonEmpty)
            solveUsesToward(g.set(log), target, k)
          else
            k(ltg)
        case noRes => k(ltg)
      }
    }
    if(lg.value.isWronglyContained(target.id))
      solveContains(lg, target, end)
    else
      end(lg.toLoggedEither)
  }

  def doMerges
  ( lg : LoggedG,
    k : LoggedTG => Unit) : Unit = {
    lg.set("\n*************** MERGES ****************")
    def aux
    ( lg : LoggedG,
      it : Iterator[ConcreteNode]
      ) : LoggedTG =
      if(it.hasNext){
        val n = it.next()
        rules.findMergingCandidate(lg.value, n) match {
          case Some(other) =>
            val ltg = rules.merge.mergeInto(lg.value, n.id, other.id)
            (lg.written <++: ltg) flatMap {
              g1 => aux(g1.set(""), g1.concreteNodes.iterator)
            }

          case None => aux(lg, it)
        }
      }
      else lg.toLoggedTry

    k(aux(lg, lg.value.concreteNodes.iterator))

  }

  def solve
  ( graph : DependencyGraph,
    k : LoggedTG => Unit) : Unit = {
    def aux: LoggedTG => Unit =
      lgt =>
        lgt.value match {
          case -\/(_) => k(lgt)
          case \/-(g) =>
            decisionMaker.violationTarget(g.set(lgt.log)) {
              loggedSTarget =>
                val lg = g.set(loggedSTarget.written)
                loggedSTarget.value match {
                case None => doMerges(lg, k)
                case Some(target) =>
                  solveViolationsToward(lg, target)(aux)
              }
            }
        }

    aux(LoggedSuccess(graph))
  }


}
