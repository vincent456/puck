package puck.graph.constraints

import puck.graph._
import puck.graph.immutable.NoType
import puck.util.{PuckLog, PuckLogger}

import scala.util.{Success, Try, Failure}

trait Solver {

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.Solver, PuckLog.Info)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) = (PuckLog.Solver, lvl)

  val logger : PuckLogger
  val decisionMaker : DecisionMaker

  type GraphT = AccessGraph
  type ResT = ResultT
  type NIdT = NodeId
  type PredicateT = decisionMaker.PredicateT

  def redirectTowardExistingAbstractions(graph : GraphT,
                                         usee : NIdT,
                                         wrongUsers : Seq[NIdT])
                                        (k : (GraphT, Seq[NIdT]) => Unit) {
    decisionMaker.abstractionKindAndPolicy(graph, usee) {
      case Some((absKind, absPolicy)) =>

        logger.writeln("redirect toward existing abstractions")
        val (g3, allUnsolved) = wrongUsers.foldLeft((graph, Seq[NIdT]())) {
          case ((g, unsolved), wu) =>
          g.abstractions(usee) find {
            case (node, `absPolicy`) if g.getNode(node).kind == absKind =>
              !graph.interloperOf(wu, node)
            case _ => false
          } match {
            case None => (g, wu +: unsolved)
            case Some((abs, _)) =>
              logger.writeln(wu + " will use abstraction " + abs)

              //val breakPoint = g.startSequence()

              g.redirectUses(AGEdge.uses(wu, usee), abs, absPolicy) match {
                  case Success((_, g2)) => (g2, unsolved)
                  case Failure(e) =>
                    logger.writeln("redirection error catched !!")(PuckLog.Debug)
                    (g/*.undo(breakPoint)*/, wu +: unsolved)
              }
          }
        }
        k(g3, allUnsolved)
      case None =>k(graph, wrongUsers)
    }
  }

  var newCterNumGen = 0

  private val allwaysTrue : PredicateT = (_,_) => true

  trait FindHostResult
  case class Host(host : NIdT, graph : GraphT) extends FindHostResult
  case class FindHostError() extends Throwable with FindHostResult

  def findHost(graph : GraphT,
               toBeContained : NIdT,
               specificPredicate : PredicateT = allwaysTrue,
               parentsThatCanBeCreated : Int = 1)
              (k : FindHostResult => Unit) {

    def predicate(graph : GraphT, n : NIdT) : Boolean =
      (graph.getNode(n) canContain toBeContained) && specificPredicate(graph, n)

    def hostIntro(toBeContainedId : NIdT) {
      logger.writeln("call hostIntro " + toBeContainedId )(PuckLog.Debug)
      val toBeContained = graph.getNode(toBeContainedId)
      graph.nodeKinds.find(_.canContain(toBeContained.kind)) match {
        case None =>
          logger.write("do not know how to create a valid host for " + toBeContained.kind)(PuckLog.Debug)
          k(FindHostError())
        case Some(hostKind) =>
          newCterNumGen += 1
          val hostName = "%s_container%d".format(toBeContained.name, newCterNumGen)
          val (hid, graph2) = graph.addNode(hostName, hostKind, NoType)
          logger.writeln("creating " + hostName )(PuckLog.Debug)
          logger.writeln("host intro, rec call to find host " + parentsThatCanBeCreated )
          findHost(graph2, hid, allwaysTrue, parentsThatCanBeCreated - 1){
            case Host(hostsHostId, graph3) =>
              k(Host(hid, graph3.addContains(hostsHostId, hid)))
            case FindHostError() =>
              logger.writeln("host intro transmit find host error" )(PuckLog.Error)
              k(FindHostError())

          }
      }
    }

    logger.writeln("find host for "+ toBeContained + ", call to choose Node "+ parentsThatCanBeCreated)(PuckLog.Debug)
    // with the search engine, all solutions will be explored anyway
    decisionMaker.chooseNode(graph, predicate){
      case None =>
        if(parentsThatCanBeCreated == 0){
          val msg = "host intro, ancestor's max limit is not enough"
          logger.writeln(msg)(PuckLog.Warning)
          k(FindHostError())
        }
        else {
          logger.writeln("find host, no node given by decision maker : call to host intro")(PuckLog.Debug)
          hostIntro(toBeContained)
        }
      case Some(h) =>
        logger.writeln("find host: decision maker chose " + h + " to contain " + toBeContained)
        k(Host(h, graph))
    }
  }

  def absIntroPredicate(graph : GraphT,
                        impl : NIdT,
                        absPolicy : AbstractionPolicy,
                        absKind : NodeKind) : PredicateT = absPolicy match {
    case SupertypeAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(impl, potentialHost)

    case DelegationAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(potentialHost, impl)
  }

  def absIntro(graph : GraphT,
               impl : NIdT,
               wrongUsers : Seq[NIdT],
               degree : Int = 1)
               (k : Try[GraphT] => Unit) : Unit = {

    logger.writeln("abs of "+ impl +" intro degree "+degree)


    def aux(graph : GraphT, deg : Int, currentImpl : NIdT)
           (k : Try[(GraphT, NIdT, AbstractionPolicy)] => Unit) : Unit = {
      logger.writeln("*** abs intro degree %d/%d ***".format(deg, degree))

      decisionMaker.abstractionKindAndPolicy(graph, currentImpl){
        case Some((absKind, absPolicy)) =>

          def doIntro(k: Try[(GraphT, NIdT, AbstractionPolicy)] => Unit) : Unit = {
            logger.writeln("trying to create abstraction( %s, %s ) of %s ".
              format(absKind, absPolicy, currentImpl))

            val tryAbs = graph.createAbstraction(currentImpl, absKind, absPolicy)

            if(tryAbs.isFailure)
              logger.writeln("failure !!")

            tryAbs map {
              case (absId, graph2) =>
                logger.writeln("in solver "+ absId + " introduced as "+ absPolicy + " for " + currentImpl)(PuckLog.ConstraintSearch, PuckLog.Debug)

                logger.writeln("Searching host for abstraction( %s, %s ) of %s ".
                  format(absKind, absPolicy, currentImpl))

                findHost(graph2, absId,
                  absIntroPredicate(graph2, currentImpl, absPolicy, absKind)) {
                  case Host(h, graph3) =>
                    logger.writeln("absIntro : host of %s is %s".format(absId, h))

                    val graph4 = graph3.addContains(h, absId)

                    //TODO check if can find another way more generic
                    //whitout passing by abstracting the container
                    val graph5 = graph4.abstractionCreationPostTreatment(currentImpl, absId, absPolicy)

                    k(Success((graph5, absId, absPolicy)))
                  case FindHostError() =>
                    logger.writeln("error while searching host for abstraction")
                    k(Failure(FindHostError()))
                }
            }

          }

          if (deg == degree)
            doIntro(k)
          else
            doIntro({
              case Failure(_) =>
                k(Failure(new AGError("Single abs intro degree %d/%d error (currentImpl = %s)".
                format(deg, degree, currentImpl))))
              case Success((g, abs, _)) => aux(g, deg + 1, abs)(k)
            })

        case None => k(Failure(new AGError("no abstraction for impl of kind " + graph.getNode(currentImpl).kind)))
      }
    }


    aux(graph, 1, impl) {
      case Success((g, abs, absPolicy)) =>

        logger.writeln("redirecting wrong users !!")
        val res = wrongUsers.foldLeft(Success(g) : Try[GraphT]){
          case (Failure(exc), wuId) => Failure(exc)
          case (Success(g0), wuId) =>
            g0.redirectUses(AGEdge.uses(wuId, impl), abs, absPolicy) match {
              case Success((_, g1)) => Success(g1)
              case Failure(e) => Failure(e)
            }
        }
        k(res)
      case Failure(e) => k(Failure(e))

    }

  }

  def solveUsesToward(graph : GraphT, impl : NIdT, k : Try[GraphT] => Unit) {
    logger.writeln("###################################################")
    logger.writeln("##### Solving uses violations toward %s ######".format(impl))

    redirectTowardExistingAbstractions(graph, impl, graph.wrongUsers(impl)){
      (graph2, wrongUsers) =>
      if (wrongUsers.nonEmpty){
        absIntro(graph2, impl, wrongUsers){
          case Failure(_) =>
            //dead code : en acceptant qu'une abstraction nouvellement introduite
            //soit la cible de violation, on a jamais besoin d'utiliser le degré 2
            absIntro(graph2, impl, wrongUsers, 2){
              case Failure(e) => k(Failure(e))

                /*decisionMaker.modifyConstraints(LiteralNodeSet(wrongUsers), impl)
                if(impl.wrongUsers.nonEmpty)
                throw new AGError ("cannot solve uses toward " + impl)
                k ()*/
              case sg => k (sg)
            }
          case sg => k (sg)
        }
      }
      else k(Success(graph2))
    }
  }

  def solveContains(graph : GraphT,
                    wronglyContained : NIdT,
                    k : Try[GraphT] => Unit) {
    logger.writeln("###################################################")
    logger.writeln("##### Solving contains violations toward %s ######".format(wronglyContained))

    // detach for host searching : do not want to consider parent constraints
    val oldCter = graph.container(wronglyContained)

    val graphWithoutContains = graph.removeContains(oldCter, wronglyContained, register = false)

    findHost(graphWithoutContains, wronglyContained,
      (graph : GraphT, potentialHost: NIdT) =>
        !graph.interloperOf(potentialHost, wronglyContained)) {
       case Host(newCter, graph2) =>

        logger.writeln("solveContains : host of "+ wronglyContained +" will now be " + newCter)



         val tryGraph3 = graph2. //re-attach before moving
                        addContains(oldCter, wronglyContained, register = false).
                        moveTo(wronglyContained, newCter)

        val tryGraph4 = tryGraph3.map {graph3 =>
          if(graph3.isWronglyContained(wronglyContained))
            graph3.addHideFromRootException(wronglyContained, newCter)
          else graph3
        }

        logger.writeln("solveContains : calling k()")
        k(tryGraph4)
       case FindHostError() => k(Failure(new AGError("FindHostError caught")))
    }
  }


  def solveViolationsToward(graph : GraphT, target : NIdT) (k: Try[GraphT] => Unit ) = {
    def end: Try[GraphT] => Unit = {
      case Success(g) =>
      logger.writeln("solveViolationsToward$end")
      if (g.wrongUsers(target).nonEmpty)
        solveUsesToward(g, target, k)
      else
        k(Success(g))
      case noRes =>k(noRes)
    }

    if(graph.isWronglyContained(target))
      solveContains(graph, target, end)
    else
      end(Success(graph))
  }


  def doMerges(graph : GraphT, k : Try[ResT] => Unit) {

    def aux(graph : GraphT, it : Iterator[NIdT]) : GraphT =
      if(it.hasNext){
        val n = it.next()
        graph.findMergingCandidate(n) match {
          case Some(other) =>
            val g1 = graph.merge(other, n)
            aux(g1, g1.nodesId.iterator)
          case None => aux(graph, it)
        }
      }
      else graph

    val g = aux(graph, graph.nodesId.iterator)

    k(Success(g, g.recording))

  }

  def solve(graph : GraphT, k : Try[ResT] => Unit) {
    /*logger.writeln("solve begins !")
    val sortedId = graph.nodesId.toSeq.sorted
    sortedId.foreach{id => logger.writeln("("+ id + ", " + graph.container(id)+ ")")}
    val nodes = graph.nodes.toSeq.sortBy(_.id)
    nodes foreach {n => logger.writeln(n)}
*/
    def aux: Try[GraphT] => Unit = {
      case Success(g) =>
      decisionMaker.violationTarget(g) {
        case None => doMerges(g, k)
        case Some(target) =>
          solveViolationsToward(g, target)(aux)
      }
      case Failure(e) => k(Failure(e))
    }

    aux(Success(graph))
  }


}
