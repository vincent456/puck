package puck.graph.constraints

import puck.graph._
import puck.util.{PuckLog, PuckLogger}

trait Solver[Kind <: NodeKind[Kind]] {

  implicit val defaultVerbosity : PuckLog.Verbosity = (PuckLog.Solver(), PuckLog.Debug())
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) = (PuckLog.Solver(), lvl)

  val graph : AccessGraph[Kind]
  val logger : PuckLogger

  val decisionMaker : DecisionMaker[Kind]

  type NodeType = AGNode[Kind]

  def redirectTowardExistingAbstractions(usee: NodeType,
                                         wrongUsers : List[NodeType])(k : List[NodeType] => Unit){
    absKindAndPolicy(usee) {
      case (absKind, absPolicy) =>

        logger.writeln("redirect toward existing abstractions")
        val allUnsolved = wrongUsers.foldLeft(List[NodeType]()) { (unsolved: List[NodeType], wu: NodeType) =>
          usee.abstractions find {
            case (node, `absPolicy`) if node.kind == absKind => !wu.interloperOf(node)
            case _ => false
          } match {
            case None => wu :: unsolved
            case Some((abs, _)) =>
              logger.writeln(wu + " will use abstraction " + abs)

              val breakPoint = graph.transformations.startSequence()

              try {
                 graph.redirectUses(AGEdge.uses(wu, usee), abs, absPolicy)
                unsolved
              }
              catch {
                case e: RedirectionError =>
                  logger.writeln("redirection error catched !!")
                  graph.transformations.undo(breakPoint)
                  wu :: unsolved
              }

          }
        }
        k(allUnsolved)
    }
  }

  var newCterNumGen = 0

  type PredicateT = (NodeType) => Boolean

  private val allwaysTrue : PredicateT = _ => true

  trait FindHostResult
  case class Host(host : NodeType) extends FindHostResult
  case class FindHostError() extends FindHostResult

  def findHost(toBeContained : NodeType,
               specificPredicate : PredicateT = allwaysTrue,
               parentsThatCanBeCreated : Int = 2)
              (k : FindHostResult => Unit) : Unit = {

    def basePredicate(n : NodeType) =
      n != toBeContained &&
        n.canContain(toBeContained)

    def predicate(n : NodeType) = basePredicate(n) && specificPredicate(n)

    def hostIntro(toBeContained : NodeType) {
      graph.nodeKinds.find(_.canContain(toBeContained.kind)) match {
        case None =>
          logger.write("do not know how to create a valid host for " + toBeContained.kind)(PuckLog.Warning())
          FindHostError()
        case Some(hostKind) =>
          newCterNumGen += 1
          val hostName = "%s_container%d".format(toBeContained.name, newCterNumGen)
          val h = graph.addNode(hostName, hostKind)
          findHost(h, specificPredicate, parentsThatCanBeCreated - 1){
            case Host(hostsHost) =>
              hostsHost.content += h
              k(Host(h))
            case FindHostError() => k(FindHostError())

          }
      }
    }

    // with the search engine, all solutions will be explored anyway
    decisionMaker.chooseNode(predicate){
      case None =>
        if(parentsThatCanBeCreated == 0){
          val msg = "host intro, ancestor's max limit is not enough"
          logger.writeln(msg)(PuckLog.Warning())
          k(FindHostError())
        }
        hostIntro(toBeContained)
      case Some(h) => k(Host(h))
    }
  }

  def absIntroPredicate(impl : NodeType,
                              absPolicy : AbstractionPolicy,
                              absKind : Kind) : PredicateT = absPolicy match {
    case SupertypeAbstraction() =>
      potentialHost => !(impl interloperOf potentialHost)

    case DelegationAbstraction() =>
      potentialHost => !(potentialHost interloperOf impl)
  }

  def absIntro(impl : NodeType,
               wrongUsers : List[NodeType],
               degree : Int = 1)
               (k : Option[NodeType] => Unit){

    logger.writeln("abs of "+ impl +" intro degree "+degree)


    def aux(deg : Int, currentImpl : NodeType)
           (k :Option[(NodeType, AbstractionPolicy)] => Unit) {
      logger.writeln("*** abs intro degree %d/%d ***".format(deg, degree))

      absKindAndPolicy(currentImpl){
        case (absKind, absPolicy) =>

          def doIntro(k: Option[(NodeType, AbstractionPolicy)] => Unit) {
            logger.writeln("Searching host for abstraction( %s, %s ) of %s ".
              format(absKind, absPolicy, currentImpl))

            val abs = currentImpl.createAbstraction(absKind, absPolicy)

            findHost(abs, absIntroPredicate(currentImpl, absPolicy, absKind)) {
              case Host(h) =>
                logger.writeln("absIntro : host of %s is %s".format(abs, h))
                h.content += abs
                k(Some(abs, absPolicy))
              case FindHostError() => ()
            }
          }

          if (deg == degree)
            doIntro(k)
          else
            doIntro({
              case None => throw new AGError("Single abs intro degree %d/%d error (currentImpl = %s)".
                format(deg, degree, currentImpl))
              case Some((abs, _)) =>
                aux(deg + 1, abs)(k)
            })
      }
    }


    aux(1, impl) {
      case None => k(None)
      case Some((abs, absPolicy)) =>

        logger.writeln("redirecting wrong users !!")
        wrongUsers.foreach(wu => graph.redirectUses(AGEdge.uses(wu, impl), abs, absPolicy))

        k(Some(abs))
    }

  }

  def absKindAndPolicy(impl : NodeType) (k : ((Kind, AbstractionPolicy)) => Unit) {
    decisionMaker.abstractionKindAndPolicy(impl) {
      case None => throw new AGError(impl + " has no abstraction policy !")
      case Some(kabs) => k(kabs)
    }
  }


  def solveUsesToward(impl : NodeType, k : () => Unit) {
    logger.writeln("###################################################")
    logger.writeln("##### Solving uses violations toward %s ######".format(impl))

    redirectTowardExistingAbstractions(impl, impl.wrongUsers){ wrongUsers =>
      if (wrongUsers.nonEmpty){
        absIntro(impl, wrongUsers){
          case None =>
            //dead code : en acceptant qu'une abstraction nouvellement introduite
            //soit la cible de violation, on a jamais besoin d'utiliser le degré 2
            absIntro(impl, wrongUsers, 2) {
              case None =>
                decisionMaker.modifyConstraints(LiteralNodeSet(wrongUsers), impl)
                /*if(impl.wrongUsers.nonEmpty)
                  throw new AGError ("cannot solve uses toward " + impl)*/
                k ()
              case _ => k ()
            }
          case _ => k ()
        }
      }
    }
  }

  def solveContains(wronglyContained : NodeType, k : () => Unit) {
    logger.writeln("###################################################")
    logger.writeln("##### Solving contains violations toward %s ######".format(wronglyContained))

    val breakPoint = graph.transformations.startSequence()
    // detach for host searching : do not want to consider parent constraints
    val oldCter = wronglyContained.container
    if(wronglyContained.container != wronglyContained)
      oldCter.content -= (wronglyContained, register = false)

    //wronglyContained.addHideFromRootException(newCter)
    logger.writeln( "Moving " + wronglyContained + ": searching a host for it !")

     findHost(wronglyContained,
      (potentialHost: NodeType) => !(potentialHost interloperOf wronglyContained)) {
       case Host(newCter) =>
        //re-attach before moving
        logger.writeln("Move : host of "+ wronglyContained +" is now " + newCter)
        if(oldCter != wronglyContained)
          oldCter.content += (wronglyContained, register = false)
        wronglyContained.moveTo(newCter)
        k()
       case FindHostError() =>
        logger.writeln("HostIntroError caught")
        decisionMaker.modifyConstraints(LiteralNodeSet(wronglyContained.container), wronglyContained)
        if(wronglyContained.isWronglyContained) {
          graph.transformations.undo(breakPoint)
          logger.writeln("Cannot solve %s contains violation".
            format(AGEdge.contains(wronglyContained.container, wronglyContained)))(PuckLog.Error())
        }
        else k()
    }
  }


  def solveViolationsToward(target : NodeType) (k: () => Unit){
    def end() =
      if(target.wrongUsers.nonEmpty)
        solveUsesToward(target, k)

    if(target.isWronglyContained)
      solveContains(target, end)
    else
      end()
  }


  def doMerges(){

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

    aux(graph.iterator)

  }

  def solve() {

    def aux(){
      decisionMaker.violationTarget {
        case None => doMerges()
        case Some(target) =>
          solveViolationsToward(target){ () =>
            //step()
            aux()
          }
      }
    }

    aux()
  }


}
