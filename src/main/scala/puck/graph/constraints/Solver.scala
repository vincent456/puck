package puck.graph
package constraints

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph.transformations.TransformationRules
import puck.util.Logged
import puck.util.LoggedEither._
import scalaz._, Scalaz._


object FindHostResult {
  def host(host : NodeId, graph : DependencyGraph): FindHostResult =
    Host(host, graph)
  def error : FindHostResult = FindHostError
}
trait FindHostResult
case class Host(host : NodeId, graph : DependencyGraph) extends FindHostResult
case object FindHostError extends DGError with FindHostResult

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
            choices : Set[(NodeId, AbstractionPolicy)]
             ) : Unit = {
      val g = lg.value
      decisionMaker.selectExistingAbstraction(lg, choices) {
        loggedOption =>
          val log = loggedOption.written
          loggedOption.value match {
            case None =>
              k((g,wrongUsers).set(log).toLoggedEither[PuckError])
            case Some((absId, absPol)) =>

              val (remainingWus, wuToRedirect) = wrongUsers.partition(g.interloperOf(_, absId))
              val lg1 = lg :++> s"$wuToRedirect will use abstraction $absId\n"

              val ltg: LoggedTG =
                wuToRedirect.foldLoggedEither(lg1) {
                  (g, wu) =>
                    rules.redirection.redirectUsesAndPropagate(g, DGEdge.UsesK(wu, used.id), absId, absPol)
                }

              ltg.value match {
                case \/-(g2) =>
                  val lg2 = ltg.valueOr(_ => sys.error("should not happen"))
                  aux(lg2, remainingWus, choices - ((absId, absPol)))
                case -\/(err) =>
                  k(LoggedError(err, ltg.log))
              }
          }
      }

    }
    aux( lg :++> "redirect toward existing abstractions\n",
          wrongUsers, lg.value.abstractions(used.id))
  }



  var newCterNumGen = 0

  private val allwaysTrue : NodePredicateT = (_,_) => true




  def findHost(lg : LoggedG,
               toBeContained : ConcreteNode,
               specificPredicate : NodePredicateT = allwaysTrue,
               parentsThatCanBeCreated : Int = 1)
              (k : Logged[FindHostResult] => Unit) : Unit = {

    object FindHostPredicate extends NodePredicate {
      def apply(graph : DependencyGraph, n : ConcreteNode) : Boolean =
        graph.canContain(n, toBeContained) && specificPredicate(graph, n)

      override def toString : String = s"Searching Host for $toBeContained"
    }


    def hostIntro(lg : LoggedG, toBeContained : ConcreteNode) : Unit = {


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
            val (host, graph2) = rules.intro(lg.value, hostName, hostKind, None)
            val log1 =
              log + s"creating $hostName host intro, rec call to find host " +
              s"($parentsThatCanBeCreated parents can be created)\n"

            findHost(graph2.set(log1),
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

    }

    val lg1 = lg :++>
      s"findHost(_, $toBeContained, _, parentsThatCanBeCreated = $parentsThatCanBeCreated)\n"

    decisionMaker.chooseNode(lg1, FindHostPredicate){
      loggedGraphAndSomeNodeId =>
        val log = loggedGraphAndSomeNodeId.written
        loggedGraphAndSomeNodeId.value match {
          case None =>
            if(parentsThatCanBeCreated == 0){
              k(FindHostResult.error.
                set( log + "host intro, ancestor's max limit is not enough\n"))
            }
            else {
              hostIntro(lg1.value.set( log + "find host, no node given by decision maker : call to host intro\n"),
                toBeContained)
            }
        case (Some((g, nid))) =>
          k(FindHostResult.host(nid, g).
            set(log + s"find host: decision maker chose ${showDG[NodeId](g).show(nid)} to contain $toBeContained\n"))
      }
    }
  }





  def absIntro(lg : LoggedG,
               impl : ConcreteNode,
               wrongUsers : List[NodeId],
               degree : Int = 1)
               (k : LoggedTry[DependencyGraph] => Unit) : Unit = {

    def aux(lg : LoggedG, deg : Int, currentImpl : ConcreteNode)
           (k : LoggedTry[(DependencyGraph, ConcreteNode, AbstractionPolicy)] => Unit) : Unit = {
      val lg1 = lg :++> s"\n*** abs intro degree $deg/$degree ***"
      decisionMaker.abstractionKindAndPolicy(lg1, currentImpl) {
        loggedSKindPolicy =>
          loggedSKindPolicy.value match {
          case Some((absKind, absPolicy)) =>

            def doIntro(k: LoggedTry[(DependencyGraph, ConcreteNode, AbstractionPolicy)] => Unit): Unit = {
              val graph = lg1.value
              val log = loggedSKindPolicy.written +
                s"\ntrying to create abstraction( $absKind, $absPolicy ) of $currentImpl"

              val tryAbs = rules.abstracter.createAbstraction(graph, currentImpl, absKind, absPolicy)


              tryAbs map {
                case (abs, graph2) =>
                  val log1 =
                    log + s"\n$abs introduced as $absPolicy for $currentImpl" +
                      s"Searching host for abstraction( $absKind, $absPolicy ) of $currentImpl"

                  findHost(graph2.set(log), abs,
                    rules.abstracter.absIntroPredicate(graph2, currentImpl, absPolicy, absKind)) {
                    logres =>
                      logres.value match {
                      case Host(h, graph3) =>
                        val lr2 = (logres :++>
                          s"\nabsIntro : host of $abs is ${showDG[NodeId](graph3).show(h)}").map{
                          _ =>
                            val graph4 = graph3.addContains(h, abs.id)

                            //TODO check if can find another way more generic
                            val graph5 = rules.abstracter.abstractionCreationPostTreatment(graph4, currentImpl.id, abs.id, absPolicy)
                            (graph5, abs, absPolicy)
                        }
                        k(lr2.toLoggedEither)
                      case FindHostError =>
                        k(LoggedError(FindHostError,
                          logres.written + "error while searching host for abstraction"))
                    }
                  }
              }
              ()
            }

            if (deg == degree) doIntro(k)
            else
              doIntro {
                ltg  =>
                  ltg.value match {
                    case -\/(_) =>
                      val err = new DGError(s"Single abs intro degree $deg/$degree error (currentImpl = $currentImpl)")
                      k(LoggedError(err, ltg.log))
                    case \/-((g, abs, _)) =>
                      aux(g.set(ltg.log), deg + 1, abs)(k)
                  }
              }

          case None =>
            k(LoggedError(new DGError(s"no abstraction for impl of kind $currentImpl"),
              loggedSKindPolicy.written))
        }
      }
    }


    def redirectWrongUsers(lgt : LoggedTry[(DependencyGraph, ConcreteNode, AbstractionPolicy)] ) : Unit =
      k(lgt.flatMap{
        case ((g, abs, absPolicy)) =>
        val lg = g.set("redirecting wrong users !!")
          wrongUsers.foldLoggedEither[PuckError, DependencyGraph](lg ){
          (g, wuId) =>
            rules.redirection.redirectUsesAndPropagate(g,
              DGEdge.UsesK(wuId, impl.id), abs.id, absPolicy)
        }
      })

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
            else k(LoggedSuccess(graph2, loggedTGraphWusers.log))
          case -\/(err) =>
            k(LoggedError(err, loggedTGraphWusers.log))
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

            val log = logres.written +
              s"solveContains : host of $wronglyContained will now be ${showDG[NodeId](g).show(newCter)}\n"



            def checkIfMoveSolveContains( tg : LoggedTG) : LoggedTG =
              tg.flatMap(g =>
                (g.isWronglyContained(wronglyContained.id), automaticConstraintLoosening) match {
                  case (false, _) => LoggedSuccess(g)
                  case (true, true) => LoggedSuccess(rules.addHideFromRootException (g, wronglyContained.id, newCter))
                  case (true, false) => LoggedError(new PuckError("constraint unsolvable"))
                })

            def k : LoggedTG => Unit = {
              ltg =>
                k0(checkIfMoveSolveContains(log <++: ltg))
              //k0 compose checkIfMoveSolveContains
            }
            //checkIfMoveSolveContains andThen k0
            //re-attach before moving

            val g2 = g.addContains(oldCter, wronglyContained.id, register = false)

            val ltg = g2.kindType(wronglyContained) match {
              case TypeMember =>
                val uses = g2.usesOfUsersOf(wronglyContained.id)

                if(rules.move.usedBySiblingsViaSelf(uses, g2, g2.getConcreteNode(oldCter)))
                  decisionMaker.createVarStrategy {
                    cvs =>
                      k(rules.move.typeMember(g2, List(wronglyContained.id), newCter, Some(cvs))(uses))
                  }
                else k(rules.move.typeMember(g2, List(wronglyContained.id), newCter, None)(uses))

              case TypeDecl =>
                k(rules.move.typeDecl (g2, wronglyContained.id, newCter))

              case _ => ???
            }

          case FindHostError =>
            k0(LoggedError(new DGError("FindHostError caught"), logres.written))
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
            val ltg = rules.mergeInto(lg.value, n.id, other.id)
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
