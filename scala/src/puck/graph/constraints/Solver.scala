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
        n.kind == topPriority && ! n.wrongUsers.isEmpty
      } match {
        case None => aux(tl)
        case res => res
      }
      case List() => graph.iterator.find{ ! _.wrongUsers.isEmpty }
    }

    aux(violationsKindPriority)
  }

  def redirectTowardExistingAbstractions(usee: AGNode,
                                         wrongUsers : List[AGNode]) = {
    wrongUsers.foldLeft(List[AGNode]()){(unsolved : List[AGNode], wu : AGNode) =>
      usee.abstractions find { case (node, _) => !wu.interloperOf(node)
      } match {
        case None => wu :: unsolved
        case Some((abs, absPolicy )) => wu redirectUses (usee, abs, absPolicy)
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


  def singleAbsIntro (impl : AGNode,
                      absPolicy : AbstractionPolicy,
                      absKind : NodeKind,
                      wrongUsers : List[AGNode]) : Boolean ={
    findHost(absKind, wrongUsers) {
      singleAbsIntroPredicate(impl, absPolicy, absKind)
    } match {
      case None => false
      case Some(host) =>
        val abs = impl.createAbstraction(absKind, absPolicy)
        host.content_+=(abs)
        wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
        true
    }
  }
  def multipleAbsIntro (impl : AGNode,
                        absPolicy : AbstractionPolicy,
                        absKind : NodeKind,
                        wrongUsers : List[AGNode]) : Boolean ={
    println("multipleAbsIntro not implemented")
    false
  }
  def hostIntro (impl : AGNode,
                 absPolicy : AbstractionPolicy,
                 absKind : NodeKind,
                 wrongUsers : List[AGNode]) : Boolean = {
    println("hostIntro")
    graph.nodeKinds.find(_.canContain(absKind)) match {
      case None => throw new AGError("container abstraction creation error")
      case Some(cterAbsKind) =>
        val cterAbs = impl.container_!.createAbstraction(cterAbsKind,
          absPolicy)
        val abs = impl.abstractions.find{
          case (existingAbs, `absPolicy`) => cterAbs.contains(existingAbs)
          case _ => false
        } match {
          case None =>
            val newAbs = impl.createAbstraction(absKind, absPolicy)
            newAbs.container = Some(cterAbs)
            newAbs
          case Some((existingAbs,_)) => existingAbs
        }

        findHost(cterAbs.kind, wrongUsers)() match {
          case None => false
          case h => cterAbs.container = h
            wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
            true
        }
    }
  }

/*  def hostIntro (impl : AGNode,
                 absPolicy : AbstractionPolicy,
                 absKind : NodeKind,
                 wrongUsers : List[AGNode]) : Boolean = {
    val abs = impl.createAbstraction(absKind, absPolicy)
    val parent = abs.createContainer()
    findHost(parent.kind, wrongUsers)() match {
      case None => false
      case h => parent.container = h
        wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
        true
    }
  }*/

  def intro(impl : AGNode, wrongUsers : List[AGNode]) {
    if(impl.kind.abstractionPolicies.isEmpty){
      throw new AGError(impl + " has no abstraction policy !")
    }
    val policies = impl.kind.abstractionPolicies
    val (absKind, absPolicy) =
      if(policies.tail.isEmpty
        && impl.kind.abstractKinds(policies.head).tail.isEmpty){
        (impl.kind.abstractKinds(policies.head).head, policies.head)
      }
      else
        decisionMaker.abstractionKindAndPolicy(impl)

    if (!singleAbsIntro(impl, absPolicy, absKind, wrongUsers)) {
      if (!multipleAbsIntro(impl, absPolicy, absKind, wrongUsers))
        if (!hostIntro(impl, absPolicy, absKind, wrongUsers)) {
          throw new AGError("cannot perform intro for " + impl)
        }
    }
  }


  def solve(step : () => Unit) {

    def solveContains() {
      graph.iterator.find(_.isWronglyContained) match {
        case None => ()
        case Some(n) =>

          val cter = n.container_!

          if(!decisionMaker.grantContainingAuth(cter, n)){
            // detach for host searching : do not want to consider parent constraints
            n.detach()

            val newCter = findHost(n.kind, List()){
              (potentialHost: AGNode) => !(potentialHost interloperOf n)
            } match {
              case None =>
                val newCter = n.createContainer()
                n.addHideFromRootException(newCter)
                newCter.container = findHost(newCter.kind, List())()
                newCter
              case Some(h) => h
            }
            //re-attach before moving
            n.container = Some(cter)
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
          val unsolved = redirectTowardExistingAbstractions(target, target.wrongUsers)
          if (!unsolved.isEmpty)
            intro(target, unsolved)

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
