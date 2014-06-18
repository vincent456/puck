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

  def findHost(toBeContained : AGNode,
               wrongUsers: List[AGNode], context : String)
              (predicate : (AGNode) => Boolean = _ => true ) : Option[AGNode] = {

    decisionMaker.chooseNode(graph, context){
      n =>
        n.canContain(toBeContained) &&
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
    println("\nsingle abs intro degree "+degree)

    def intro (currentImpl : AGNode, wrongUsers : List[AGNode]) : Option[(AGNode, AbstractionPolicy)] ={
      val (absKind, absPolicy) = getAbsKindAndPolicy(currentImpl)
      graph.register {
        val abs = currentImpl.createAbstraction(absKind, absPolicy)
        findHost(abs, wrongUsers,
          "Searching host for %s\n abstraction of %s ( %s )".format(abs, currentImpl, absPolicy)) {
          singleAbsIntroPredicate(currentImpl, absPolicy, absKind)
        } match {
          case None =>

            graph.transformations.undo()
            println("host not found, trying hostIntro")
            hostIntro(currentImpl, absKind, absPolicy, wrongUsers)
          case Some(host) =>
            println("found host : " + host)
            host.content_+=(abs)
            Some(abs, absPolicy)
        }
      }
    }

    def hostIntro (toBeContained : AGNode,
                   absKind : NodeKind,
                   absPolicy : AbstractionPolicy,
                   wrongUsers : List[AGNode]) : Option[(AGNode, AbstractionPolicy)] = {
      println("\nhostIntro")
      toBeContained.container.kind.abstractKinds(absPolicy).find(_.canContain(absKind)) match {
        case None => throw new AGError("container abstraction creation error")
        case Some(cterAbsKind) =>
            val cterAbs = toBeContained.container.createAbstraction(cterAbsKind,
              absPolicy)
            val abs = toBeContained.abstractions.find {
              case (existingAbs, `absPolicy`) => cterAbs.contains(existingAbs)
              case _ => false
            } match {
              case None =>
                val newAbs = toBeContained.createAbstraction(absKind, absPolicy)
                cterAbs content_+= newAbs
                newAbs
              case Some((existingAbs, _)) => existingAbs
            }

            findHost(cterAbs, wrongUsers,
              ("%s abstracted as\n %s (%s)\n%s (%s) was introduced to contains it.\n" +
                "Searching a container for %s").format(toBeContained, abs, absPolicy, cterAbs, absPolicy, cterAbs)
            )( n => n != cterAbs) match {
              case None => None
              case Some(h) =>
                h content_+= cterAbs
                Some(abs, absPolicy)
            }
      }
    }

    def aux(deg : Int, currentImpl : AGNode) : Option[(AGNode, AbstractionPolicy)] = {
      println("*** abs intro degree %d/%d ***".format(deg,degree))
      if(deg == degree)
        intro (currentImpl, wrongUsers)
      else
        intro (currentImpl, List()) match {
          case None => throw new AGError("Single abs intro degree %d/%d error".format(deg, degree))
          case Some((abs, _)) =>
            aux(deg + 1, abs)
        }
    }
    graph.register {
      aux(1, impl) match {
        case None => None
        case Some((abs, absPolicy)) =>

          wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
          if(impl.wrongUsers.nonEmpty){
            println("abs intro failure, remaing wrongusers :")
            print(impl.wrongUsers.mkString("-","\n-", "\n"))
            graph.transformations.undo()
            None
          }
          else{
            Some(abs)
          }
      }
    }
  }

  def multipleAbsIntro (impl : AGNode,
                        wrongUsers : List[AGNode]) : Boolean ={
    println("\nmultipleAbsIntro not implemented")
    false
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
    println("###################################################")
    println("##### Solving uses violations toward %s ######".format(impl))
    val wrongUsers = redirectTowardExistingAbstractions(impl, impl.wrongUsers)

    if (wrongUsers.nonEmpty){
      singleAbsIntro(impl, wrongUsers) match {
        case None =>
          if (!multipleAbsIntro(impl, wrongUsers))
            singleAbsIntro(impl, wrongUsers, 2) match {
              case None =>
                throw new AGError ("cannot solve uses toward " + impl)
              case _ => ()
            }
        case _ => ()
      }
    }
  }


  def solve(step : () => Unit) {

    def solveContains() {
      graph.iterator.find(_.isWronglyContained) match {
        case None => ()
        case Some(wronglyContained) =>

          val cter = wronglyContained.container

          if(!decisionMaker.grantContainingAuth(cter, wronglyContained)){
            // detach for host searching : do not want to consider parent constraints
            wronglyContained.detach()

            val newCter = findHost(wronglyContained, List(),
              "Moving " + wronglyContained +"\nSearching a host for it"){
              (potentialHost: AGNode) => !(potentialHost interloperOf wronglyContained) &&
                potentialHost != wronglyContained
            } match {
              case None =>
                val newCter = graph.nodeKinds.find(_.canContain(wronglyContained.kind)) match {
                  case None => throw new AGError("do not know how to create a valid parent for " + wronglyContained.kind)
                  case Some(parentKind) =>
                    graph.addNode (wronglyContained.name + "_container", parentKind)
                }

                wronglyContained.addHideFromRootException(newCter)

                findHost(newCter, List(),
                  "Moving " + wronglyContained +"\n"+
                    newCter+" created to contain it.\n" +
                    "Searching a host for " + newCter)( n => n != newCter) match {
                  case None => throw new AGError("Cannot find a container's container")
                  case Some(h) =>
                    graph.root.content_-=(newCter)
                    h.content_+=(newCter)
                    newCter
                }
              case Some(h) => h
            }
            //re-attach before moving
            cter content_+= wronglyContained
            wronglyContained.moveTo(newCter)
          }
          step()
          solveContains()
      }
    }

    def solveUses() {
      priorityViolationTarget match {
        case None => ()
        case Some(target) =>
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
