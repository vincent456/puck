package puck.graph.constraints

import puck.graph._
import puck.graph.immutable.NoType
import puck.util.{PuckLog, PuckLogger}

trait Solver[Kind <: NodeKind[Kind], T] {

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.Solver, PuckLog.Info)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) = (PuckLog.Solver, lvl)

  val logger : PuckLogger
  val decisionMaker : DecisionMaker[Kind, T]

  type GraphT = AccessGraph[Kind, T]
  type ResT = ResultT[Kind, T]
  type NIdT = NodeId[Kind]
  type PredicateT = decisionMaker.PredicateT

  def redirectTowardExistingAbstractions(graph : GraphT,
                                         usee : NIdT,
                                         wrongUsers : Seq[NIdT])
                                        (k : (GraphT, Seq[NIdT]) => Option[ResT]) : Option[ResT] = {
    decisionMaker.abstractionKindAndPolicy(graph, usee) {
      case (absKind, absPolicy) =>

        logger.writeln("redirect toward existing abstractions")
        val (g3, allUnsolved) = wrongUsers.foldLeft((graph, Seq[NIdT]())) {
          case ((g,unsolved), wu) =>
          g.abstractions(usee) find {
            case (node, `absPolicy`) if g.getNode(node).kind == absKind =>
              !graph.interloperOf(wu, node)
            case _ => false
          } match {
            case None => (g, wu +: unsolved)
            case Some((abs, _)) =>
              logger.writeln(wu + " will use abstraction " + abs)

              val breakPoint = g.startSequence()

              try {
                val (_, g2) = g.redirectUses(AGEdge.uses(wu, usee), abs, absPolicy)
                (g2, unsolved)
              }
              catch {
                case e: RedirectionError =>
                  logger.writeln("redirection error catched !!")(PuckLog.Debug)
                  (g.undo(breakPoint), wu +: unsolved)
              }

          }
        }
        k(g3, allUnsolved)
    }
  }

  var newCterNumGen = 0

  private val allwaysTrue : PredicateT = (_,_) => true

  trait FindHostResult
  case class Host(host : NIdT, graph : GraphT) extends FindHostResult
  case class FindHostError() extends FindHostResult

  def findHost(graph : GraphT,
               toBeContained : NIdT,
               specificPredicate : PredicateT = allwaysTrue,
               parentsThatCanBeCreated : Int = 1)
              (k : FindHostResult => Option[ResT]) : Option[ResT] = {

    def predicate(graph : GraphT, n : NIdT) : Boolean =
      (graph.getNode(n) canContain toBeContained) && specificPredicate(graph, n)

    def hostIntro(toBeContainedId : NIdT) : Option[ResT] = {
      logger.writeln("call hostIntro " + toBeContainedId )(PuckLog.Debug)
      val toBeContained = graph.getNode(toBeContainedId)
      graph.nodeKinds.find(_.canContain(toBeContained.kind)) match {
        case None =>
          logger.write("do not know how to create a valid host for " + toBeContained.kind)(PuckLog.Debug)
          k(FindHostError())
        case Some(hostKind) =>
          newCterNumGen += 1
          val hostName = "%s_container%d".format(toBeContained.name, newCterNumGen)
          val (hid, graph2) = graph.addNode(hostName, hostKind, NoType())
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
                        absKind : Kind) : PredicateT = absPolicy match {
    case SupertypeAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(impl, potentialHost)

    case DelegationAbstraction =>
      (graph, potentialHost) => !graph.interloperOf(potentialHost, impl)
  }

  def absIntro(graph : GraphT,
               impl : NIdT,
               wrongUsers : Seq[NIdT],
               degree : Int = 1)
               (k : Option[GraphT] => Option[ResT]) : Option[ResT] = {

    logger.writeln("abs of "+ impl +" intro degree "+degree)


    def aux(graph : GraphT, deg : Int, currentImpl : NIdT)
           (k : Option[(GraphT, NIdT, AbstractionPolicy)] => Option[ResT]) : Option[ResT] = {
      logger.writeln("*** abs intro degree %d/%d ***".format(deg, degree))

      decisionMaker.abstractionKindAndPolicy(graph, currentImpl){
        case Some((absKind, absPolicy)) =>

          def doIntro(k: Option[(GraphT, NIdT, AbstractionPolicy)] => Option[ResT]) : Option[ResT] = {
            logger.writeln("Searching host for abstraction( %s, %s ) of %s ".
              format(absKind, absPolicy, currentImpl))

            val (absId, graph2) = graph.createAbstraction(currentImpl, absKind, absPolicy)

            logger.writeln("in solver "+ absId + " introduced as "+ absPolicy + " for " + currentImpl)(PuckLog.Search, PuckLog.Debug)

            findHost(graph2, absId,
                     absIntroPredicate(graph2, currentImpl, absPolicy, absKind)) {
              case Host(h, graph3) =>
                logger.writeln("absIntro : host of %s is %s".format(absId, h))

                val graph4 = graph3.addContains(h, absId)

                //TODO check if can find another way more generic
                //whitout passing by abstracting the container
                val graph5 = graph4.abstractionCreationPostTreatment(currentImpl, absId, absPolicy)

                k(Some(graph5, absId, absPolicy))
              case FindHostError() =>
                logger.writeln("error while searching host for abstraction")
                k(None)
            }
          }

          if (deg == degree)
            doIntro(k)
          else
            doIntro({
              case None => throw new AGError("Single abs intro degree %d/%d error (currentImpl = %s)".
                format(deg, degree, currentImpl))
              case Some((g, abs, _)) =>
                aux(g, deg + 1, abs)(k)
            })

        case None => k(None)
      }
    }


    aux(graph, 1, impl) {
      case None => k(None)
      case Some((g, abs, absPolicy)) =>

        logger.writeln("redirecting wrong users !!")
        val g2 = wrongUsers.foldLeft(g){
          case (g0, wuId) =>
            val (_, g1) = g0.redirectUses(AGEdge.uses(wuId, impl), abs, absPolicy)
            g1
        }
        k(Some(g2))
    }

  }

  def solveUsesToward(graph : GraphT, impl : NIdT, k : Option[GraphT] => Option[ResT]) : Option[ResT] = {
    logger.writeln("###################################################")
    logger.writeln("##### Solving uses violations toward %s ######".format(impl))

    redirectTowardExistingAbstractions(graph, impl, graph.wrongUsers(impl)){
      case (graph2, wrongUsers) =>
      if (wrongUsers.nonEmpty){
        absIntro(graph2, impl, wrongUsers){
          case None =>
            //dead code : en acceptant qu'une abstraction nouvellement introduite
            //soit la cible de violation, on a jamais besoin d'utiliser le degré 2
            absIntro(graph2, impl, wrongUsers, 2){
              case None => k(None)

                /*decisionMaker.modifyConstraints(LiteralNodeSet(wrongUsers), impl)
                if(impl.wrongUsers.nonEmpty)
                throw new AGError ("cannot solve uses toward " + impl)
                k ()*/
              case sg => k (sg)
            }
          case sg => k (sg)
        }
      }
      else k(Some(graph2))
    }
  }

  def solveContains(graph : GraphT,
                    wronglyContained : NIdT,
                    k : Option[GraphT] => Option[ResT]) : Option[ResT] = {
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



         val graph3 = graph2. //re-attach before moving
                        addContains(oldCter, wronglyContained, register = false).
                        moveTo(wronglyContained, newCter)

        val graph4 = if(graph3.isWronglyContained(wronglyContained))
          graph3.addHideFromRootException(wronglyContained, newCter)
        else graph3

        logger.writeln("solveContains : calling k()")
        k(Some(graph4))
       case FindHostError() =>
        logger.writeln("FindHostError caught")
        k(None)
    }
  }


  def solveViolationsToward(graph : GraphT, target : NIdT) (k: Option[GraphT] => Option[ResT] ) : Option[ResT] = {
    def end: Option[GraphT] => Option[ResT] = {
      case Some(g) =>
      logger.writeln("solveViolationsToward$end")
      if (g.wrongUsers(target).nonEmpty)
        solveUsesToward(g, target, k)
      else
        k(Some(g))
      case None =>k(None)
    }

    if(graph.isWronglyContained(target))
      solveContains(graph, target, end)
    else
      end(Some(graph))
  }


  def doMerges(graph : GraphT) : Option[ResT] = ??? /*{

    def aux(it : Iterator[AGNode[Kind]]) : Unit =
      if(it.hasNext){
        val n = it.next()
        n.findMergingCandidate() match {
          case Some(other) =>
            other mergeWith n
            aux(graph.iterator)
          case None => aux(it)
        }
      }
      else None

    aux(graph.nodes.iterator)

  }*/

  def solve(graph : GraphT) : Option[ResT] = {
    logger.writeln("solve begins !")
    val sortedId = graph.nodesId.toSeq.sorted
    sortedId.foreach{id => logger.writeln("("+ id + ", " + graph.container(id)+ ")")}
    val nodes = graph.nodes.toSeq.sortBy(_.id)
    nodes foreach {n => logger.writeln(n)}

    def aux: Option[GraphT] => Option[ResT] = {
      case Some(g) =>
      decisionMaker.violationTarget(g) {
        case None => doMerges(g)
        case Some(target) =>
          solveViolationsToward(g, target)(aux)
      }
      case None => None
    }

    aux(Some(graph))
  }


}
