package puck.graph
package constraints

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph.transformations.TransformationRules
import puck.util.PuckLog

import scalaz.{-\/, \/-}
import puck.util.Collections.traverse

class Solver
( val decisionMaker : DecisionMaker,
  val rules : TransformationRules,
  val automaticConstraintLoosening : Boolean){

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.Solver, PuckLog.Info)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity= (PuckLog.Solver, lvl)


  type ResT = ResultT

  def redirectTowardExistingAbstractions(graph : DependencyGraph,
                                         used : ConcreteNode,
                                         wrongUsers : Seq[NodeId])
                                        (k : Try[(DependencyGraph, Seq[NodeId])] => Unit) : Unit = {
    import graph.logger
    logger.writeln("redirect toward existing abstractions")
    def aux(g : DependencyGraph, wrongUsers : Seq[NodeId], choices : Set[(NodeId, AbstractionPolicy)]) : Unit = {
      decisionMaker.selectExistingAbstraction(g, choices){
          case None => k(\/-((g, wrongUsers)))
          case Some((absId, absPol)) =>
            val (remainingWus, wuToRedirect) = wrongUsers.partition(g.interloperOf(_, absId))

            logger.writeln(s"$wuToRedirect will use abstraction $absId")

            val tg = traverse(wuToRedirect, g) { (g, wu) =>
              rules.redirection.redirectUsesAndPropagate(g, DGEdge.uses(wu, used.id), absId, absPol)
            } match {
              case \/-(g2) => aux(g2, remainingWus, choices - ((absId, absPol)))
              case -\/(err) => k(-\/(err))
            }
      }
    }

    aux(graph, wrongUsers, graph.abstractions(used.id))

  }



  var newCterNumGen = 0

  private val allwaysTrue : NodePredicateT = (_,_) => true

  trait FindHostResult
  case class Host(host : NodeId, graph : DependencyGraph) extends FindHostResult
  case object FindHostError extends DGError with FindHostResult


  def findHost(graph : DependencyGraph,
               toBeContained : ConcreteNode,
               specificPredicate : NodePredicateT = allwaysTrue,
               parentsThatCanBeCreated : Int = 1)
              (k : FindHostResult => Unit) : Unit = {
    import graph.logger

    object FindHostPredicate extends NodePredicate {
      def apply(graph : DependencyGraph, n : ConcreteNode) : Boolean =
        graph.canContain(n, toBeContained) && specificPredicate(graph, n)

      override def toString : String = s"Searching Host for $toBeContained"
    }


    def hostIntro(graph : DependencyGraph, toBeContained : ConcreteNode) : Unit = {

      logger.writeln(s"call hostIntro $toBeContained" )(PuckLog.Debug)

      decisionMaker.chooseContainerKind(graph, toBeContained){
        case None =>
          logger.write("do not know how to create a valid host for " + toBeContained.kind)(PuckLog.Debug)
          k(FindHostError)
        case Some(hostKind) =>
          newCterNumGen += 1
          val hostName = s"${toBeContained.name}_container$newCterNumGen"
          val (host, graph2) = rules.intro.createNode(graph, hostName, hostKind, None)
          logger.writeln(s"creating $hostName host intro, rec call to find host " +
            s"($parentsThatCanBeCreated parents can be created) " )
          findHost(graph2, host, allwaysTrue, parentsThatCanBeCreated - 1){
            case Host(hostsHostId, graph3) =>
              k(Host(host.id, graph3.addContains(hostsHostId, host.id)))
            case FindHostError =>
              logger.writeln("host intro transmit find host error" )(PuckLog.Error)
              k(FindHostError)

          }
      }
    }

    logger.writeln(s"findHost(_, $toBeContained, _, parentsThatCanBeCreated = $parentsThatCanBeCreated)")(PuckLog.Debug)
    // with the search engine, all solutions will be explored anyway

    decisionMaker.chooseNode(graph, FindHostPredicate){ graph2 =>
      {case None =>
          if(parentsThatCanBeCreated == 0){
            logger.writeln("host intro, ancestor's max limit is not enough")(PuckLog.Warning)
            k(FindHostError)
          }
          else {
            logger.writeln("find host, no node given by decision maker : call to host intro")(PuckLog.Debug)
            hostIntro(graph2, toBeContained)
          }
        case Some(nid) =>
          logger.writeln(s"find host: decision maker chose ${showDG[NodeId](graph2).show(nid)} to contain $toBeContained")(PuckLog.Debug)
          k(Host(nid, graph2))
      }
    }
  }





  def absIntro(graph : DependencyGraph,
               impl : ConcreteNode,
               wrongUsers : Seq[NodeId],
               degree : Int = 1)
               (k : Try[DependencyGraph] => Unit) : Unit = {
    import graph.logger

    logger.writeln(s"abs of $impl intro degree $degree")


    def aux(graph : DependencyGraph, deg : Int, currentImpl : ConcreteNode)
           (k : Try[(DependencyGraph, ConcreteNode, AbstractionPolicy)] => Unit) : Unit = {
      logger.writeln(s"*** abs intro degree $deg/$degree ***")

      decisionMaker.abstractionKindAndPolicy(graph, currentImpl){
        case Some((absKind, absPolicy)) =>

          def doIntro(k: Try[(DependencyGraph, ConcreteNode, AbstractionPolicy)] => Unit) : Unit = {
            logger.writeln(s"trying to create abstraction( $absKind, $absPolicy ) of $currentImpl")

            val tryAbs = rules.createAbstraction(graph, currentImpl, absKind, absPolicy)

            if(tryAbs.isLeft)
              logger.writeln("failure !!")

            tryAbs map {
              case (abs, graph2) =>
                logger.writeln(s"$abs introduced as $absPolicy for $currentImpl")((PuckLog.Solver, PuckLog.Debug))
                logger.writeln(s"Searching host for abstraction( $absKind, $absPolicy ) of $currentImpl")

                findHost(graph2, abs,
                  rules.absIntroPredicate(graph2, currentImpl, absPolicy, absKind)) {
                  case Host(h, graph3) =>
                    logger.writeln(s"absIntro : host of $abs is ${showDG[NodeId](graph3).show(h)}")

                    val graph4 = graph3.addContains(h, abs.id)

                    //TODO check if can find another way more generic
                    val graph5 = rules.abstractionCreationPostTreatment(graph4, currentImpl.id, abs.id, absPolicy)

                    k(\/-((graph5, abs, absPolicy)))
                  case FindHostError =>
                    logger.writeln("error while searching host for abstraction")
                    k(-\/(FindHostError))
                }
            }
            ()
          }

          if (deg == degree)
            doIntro(k)
          else
            doIntro({
              case -\/(_) =>
                k(-\/(new DGError(s"Single abs intro degree $deg/$degree error (currentImpl = $currentImpl)")))
              case \/-((g, abs, _)) => aux(g, deg + 1, abs)(k)
            })

        case None => k(-\/(new DGError(s"no abstraction for impl of kind $currentImpl")))
      }
    }


    def redirectWrongUsers(t : Try[(DependencyGraph, ConcreteNode, AbstractionPolicy)] ) : Unit =
      k(t.flatMap{ case (g, abs, absPolicy) =>
        logger.writeln("redirecting wrong users !!")
        wrongUsers.foldLeft(\/-(g) : Try[DependencyGraph]){
          (tryG, wuId) =>
            tryG.flatMap(rules.redirection.redirectUsesAndPropagate(_, DGEdge.uses(wuId, impl.id), abs.id, absPolicy))
        }})

    aux (graph, 1, impl) (redirectWrongUsers)
  }

  def solveUsesToward(graph : DependencyGraph, impl : ConcreteNode, k : Try[DependencyGraph] => Unit) : Unit = {
    import graph.logger

    logger.writeln("###################################################")
    logger.writeln(s"##### Solving uses violations toward $impl ######")

    redirectTowardExistingAbstractions(graph, impl, graph.wrongUsers(impl.id)){
      case \/-((graph2, wrongUsers)) =>
        if (wrongUsers.nonEmpty) absIntro(graph2, impl, wrongUsers)(k)
        else k(\/-(graph2))
      case -\/(err) => k(-\/(err))

    }
  }

  def solveContains(graph : DependencyGraph,
                    wronglyContained : ConcreteNode,
                    k0 : Try[DependencyGraph] => Unit) : Unit = {
    import graph.logger

    logger.writeln("###################################################")
    logger.writeln(s"##### Solving contains violations toward $wronglyContained ######")

    // detach for host searching : do not want to consider parent constraints
    val oldCter = graph.container(wronglyContained.id).get

    val graphWithoutContains = graph.removeContains(oldCter, wronglyContained.id, register = false)

    
    findHost(graphWithoutContains, wronglyContained,
      (graph : DependencyGraph, potentialHost: ConcreteNode) =>
        !graph.interloperOf(potentialHost.id, wronglyContained.id)) {
       case Host(newCter, graph2) =>

        logger.writeln(s"solveContains : host of $wronglyContained will now be ${showDG[NodeId](graph2).show(newCter)}")


         //re-attach before moving
         val g3 = graph2.addContains(oldCter, wronglyContained.id, register = false)

         def checkIfMoveSolveContains( tg : Try[DependencyGraph]) : Try[DependencyGraph] =
         tg.flatMap(g =>
           (g.isWronglyContained(wronglyContained.id), automaticConstraintLoosening) match {
             case (false, _) => \/-(g)
             case (true, true) => \/-(rules.addHideFromRootException (g, wronglyContained.id, newCter))
             case (true, false) => -\/(new PuckError("constraint unsolvable"))
           })

         def k : Try[DependencyGraph] => Unit =
          k0 compose checkIfMoveSolveContains
          //checkIfMoveSolveContains andThen k0

         graph.kindType(wronglyContained) match {
           case TypeMember =>
             if(rules.move.isUsedBySiblingsViaSelf(g3, wronglyContained, g3.getConcreteNode(oldCter)))
              decisionMaker.createVarStrategy {
                cvs => k(rules.move.typeMember(g3, wronglyContained.id, newCter, Some(cvs)))
              }
             else k(rules.move.typeMember(g3, wronglyContained.id, newCter, None))

           case TypeDecl =>
             k(rules.move.typeDecl (g3, wronglyContained.id, newCter))

           case _ => ???
         }

       case FindHostError => k0(-\/(new DGError("FindHostError caught")))
    }
  }


  def solveViolationsToward(graph : DependencyGraph, target : ConcreteNode) (k: Try[DependencyGraph] => Unit ) = {
    def end: Try[DependencyGraph] => Unit = {
      case \/-(g) =>
      graph.logger.writeln(s"solveViolationsToward $target end")
      if (g.wrongUsers(target.id).nonEmpty)
        solveUsesToward(g, target, k)
      else
        k(\/-(g))
      case noRes =>k(noRes)
    }

    if(graph.isWronglyContained(target.id))
      solveContains(graph, target, end)
    else
      end(\/-(graph))
  }

  def doMerges(graph : DependencyGraph, k : Try[ResT] => Unit) : Unit = {
    graph.logger.writeln(s"*************** MERGES ****************")
    def aux(graph : DependencyGraph, it : Iterator[ConcreteNode]) : Try[DependencyGraph] =
      if(it.hasNext){
        val n = it.next()
        rules.findMergingCandidate(graph, n) match {
          case Some(other) =>
            rules.mergeInto(graph, n.id, other.id) flatMap {
              g1 => aux(g1, g1.concreteNodes.iterator)
            }

          case None => aux(graph, it)
        }
      }
      else \/-(graph)

    k(aux(graph, graph.concreteNodes.iterator) map (g => (g, g.recording)))

  }

  def solve(graph : DependencyGraph, k : Try[ResT] => Unit) : Unit = {
    /*logger.writeln("solve begins !")
    val sortedId = graph.nodesId.toSeq.sorted
    sortedId.foreach{id => logger.writeln("("+ id + ", " + graph.container(id)+ ")")}
    val nodes = graph.nodes.toSeq.sortBy(_.id)
    nodes foreach {n => logger.writeln(n)}
*/
    def aux: Try[DependencyGraph] => Unit = {
      case \/-(g) =>
      decisionMaker.violationTarget(g) {
        case None => doMerges(g, k)
        case Some(target) =>
          solveViolationsToward(g, target)(aux)
      }
      case -\/(e) => k(-\/(e))
    }

    aux(\/-(graph))
  }


}
