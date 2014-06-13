package puck.graph.constraints

import puck.graph._
import scala.Some

trait Solver {

  val graph : AccessGraph

  val violationsKindPriority : List[NodeKind]

  val decisionMaker : DecisionMaker

  def priorityViolationTarget : Option[AGNode] = {

    def aux (priorities : List[NodeKind]) : Option[AGNode] = priorities match {
      case topPriority :: tl => graph.iterator.find{ (n : AGNode) =>
        n.kind == topPriority && n.wrongUsers.nonEmpty
      } match {
        case None => aux(tl)
        case res => res
      }
      case List() => graph.iterator.find{ _.wrongUsers.nonEmpty }
    }

    aux(violationsKindPriority)
  }

  def redirectTowardExistingAbstractions(usee: AGNode,
                                         wrongUsers : List[AGNode]) = {
    val (absKind, absPolicy) = getAbsKindAndPolicy(usee)

    println("redirect toward existing abstractions")
    wrongUsers.foldLeft(List[AGNode]()){(unsolved : List[AGNode], wu : AGNode) =>
      usee.abstractions find {
        case (node, `absPolicy`) if node.kind == absKind => !wu.interloperOf(node)
        case _ => false
      } match {
        case None => wu :: unsolved
        case Some((abs, _ )) =>
          println(wu + " will use abstraction " + abs)
          wu redirectUses (usee, abs, absPolicy)
          unsolved
      }
    }
  }

  def findHost(kind : NodeKind,
               wrongUsers: List[AGNode])
              (predicate : (AGNode) => Boolean = _ => true ) : Option[AGNode] = {

    decisionMaker.chooseNode(graph){
      n =>
        n.kind.canContain(kind) &&
          predicate(n) &&
          wrongUsers.forall(!_.interloperOf(n))
    }
  }


  def singleAbsIntroPredicate(impl : AGNode,
                              absPolicy : AbstractionPolicy,
                              absKind : NodeKind) : AGNode => Boolean = absPolicy match {
    case SupertypeAbstraction() =>
      potentialHost => !(impl interloperOf potentialHost)
    case DelegationAbstraction() =>
      potentialHost => !(potentialHost interloperOf impl)
  }




  def singleAbsIntro(impl : AGNode,
                     wrongUsers : List[AGNode],
                     degree : Int = 1) : Option[AGNode] = {

    def intro (currentImpl : AGNode, wrongUsers : List[AGNode]) : Option[AGNode] ={
      val (absKind, absPolicy) = getAbsKindAndPolicy(currentImpl)
      println("single abs intro")
      findHost(absKind, wrongUsers) {
        singleAbsIntroPredicate(currentImpl, absPolicy, absKind)
      } match {
        case None => None
        case Some(host) =>
          println("found host for abs : " + host)
          val abs = currentImpl.createAbstraction(absKind, absPolicy)
          host.content_+=(abs)
          // /!\ redirect from original impl !
          // wrongUsers not empty only when degree == deg
          wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
          Some(abs)
      }
    }

    def aux(deg : Int, currentImpl : AGNode) : Option[AGNode] = {
        if(deg == degree)
          intro (currentImpl, wrongUsers)
        else
          intro (currentImpl, List()) match {
            case None => throw new AGError("Single abs intro degree %d/%d error".format(deg, degree))
            case Some(abs) =>
              aux(deg + 1, abs)
          }
    }
    aux(1, impl)
  }

  def multipleAbsIntro (impl : AGNode,
                        wrongUsers : List[AGNode]) : Boolean ={
    println("multipleAbsIntro not implemented")
    false
  }

  def hostIntro (impl : AGNode,
                 wrongUsers : List[AGNode]) : Boolean = {
    val (absKind, absPolicy) = getAbsKindAndPolicy(impl)
    println("hostIntro")
    //graph.nodeKinds.find(_.canContain(absKind))
    impl.container.kind.abstractKinds(absPolicy).find(_.canContain(absKind)) match {
      case None => throw new AGError("container abstraction creation error")
      case Some(cterAbsKind) =>
        graph.register[Boolean]{

          val cterAbs = impl.container.createAbstraction(cterAbsKind,
            absPolicy)
          val abs = impl.abstractions.find {
            case (existingAbs, `absPolicy`) => cterAbs.contains(existingAbs)
            case _ => false
          } match {
            case None =>
              val newAbs = impl.createAbstraction(absKind, absPolicy)
              cterAbs content_+= newAbs
              newAbs
            case Some((existingAbs, _)) => existingAbs
          }

          val success = findHost(cterAbs.kind, wrongUsers)() match {
            case None => false
            case Some(h) =>
              h content_+= cterAbs
              wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
              impl.wrongUsers.isEmpty
          }

          if(!success)
            graph.transformations.undo()

          success
        }
    }
  }

  def getAbsKindAndPolicy(impl : AGNode) : (NodeKind, AbstractionPolicy) = {
    if(impl.kind.abstractionPolicies.isEmpty){
      throw new AGError(impl + " has no abstraction policy !")
    }
    val policies = impl.kind.abstractionPolicies
      if(policies.tail.isEmpty
        && impl.kind.abstractKinds(policies.head).tail.isEmpty){
        (impl.kind.abstractKinds(policies.head).head, policies.head)
      }
      else
        decisionMaker.abstractionKindAndPolicy(impl)
  }

  def solveUsesToward(impl : AGNode) {

    val wrongUsers = redirectTowardExistingAbstractions(impl, impl.wrongUsers)

    if (wrongUsers.nonEmpty){
      singleAbsIntro(impl, wrongUsers) match {
        case None =>
          if (!multipleAbsIntro(impl, wrongUsers))
            if (!hostIntro(impl, wrongUsers)) {
              singleAbsIntro(impl, wrongUsers, 2) match {
                case None => throw new AGError ("cannot perform intro for " + impl)
                case _ => ()
              }
            }
        case _ => ()
      }
    }
  }


  def solve(step : () => Unit) {

    def solveContains() {
      graph.iterator.find(_.isWronglyContained) match {
        case None => ()
        case Some(n) =>

          val cter = n.container

          if(!decisionMaker.grantContainingAuth(cter, n)){
            // detach for host searching : do not want to consider parent constraints
            n.container.content_-=(n)

            val newCter = findHost(n.kind, List()){
              (potentialHost: AGNode) => !(potentialHost interloperOf n)
            } match {
              case None =>
                val newCter = n.createContainer()
                n.addHideFromRootException(newCter)

                findHost(newCter.kind, List())() match {
                  case None => throw new AGError("Cannot find a container's container")
                  case Some(h) =>
                    h.content_+=(newCter)
                    newCter
                }
              case Some(h) => h
            }
            //re-attach before moving
            cter content_+= n
            n.moveTo(newCter)
          }
          step()
          solveContains()
      }
    }

    def solveUses() {
      priorityViolationTarget match {
        case None => ()
        case Some(target) =>
          target.searchExistingAbstractions()
          solveUsesToward(target)
          step()
          solveUses()
      }
    }
    println("solving contains violations")
    solveContains()
    println("solving uses violations")
    solveUses()
  }

}
