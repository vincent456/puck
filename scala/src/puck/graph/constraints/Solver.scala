package puck.graph.constraints

import puck.graph._

trait SolverBuilder{
  def apply(g : AccessGraph, dm : DecisionMaker) : Solver
}

trait Solver {

  val graph : AccessGraph

  val decisionMaker : DecisionMaker

  def redirectTowardExistingAbstractions(usee: AGNode,
                                         wrongUsers : List[AGNode]) = {
    val (absKind, absPolicy) = getAbsKindAndPolicy(usee)

    println("redirect toward existing abstractions")
      wrongUsers.foldLeft(List[AGNode]()) { (unsolved: List[AGNode], wu: AGNode) =>
        usee.abstractions find {
          case (node, `absPolicy`) if node.kind == absKind => !wu.interloperOf(node)
          case _ => false
        } match {
          case None => wu :: unsolved
          case Some((abs, _)) =>
            println(wu + " will use abstraction " + abs)
            graph.register {
              try {
                wu redirectUses(usee, abs, absPolicy)
                unsolved
              }
              catch{
                case e :RedirectionError =>
                  graph.transformations.undo()
                  wu :: unsolved
              }

            }
        }
      }

  }

  def findHost(toBeContained : AGNode,
               wrongUsers: List[AGNode], context : => String)
              (predicate : (AGNode) => Boolean = _ => true ) : Option[AGNode] = {

    def loosePredicate(n : AGNode) =
      n != toBeContained &&
        n.canContain(toBeContained) &&
        predicate(n)

    def strictPredicate( n :AGNode ) = loosePredicate(n) &&
      wrongUsers.forall(!_.interloperOf(n))

    decisionMaker.chooseNode(context)(strictPredicate) match {
      case None => decisionMaker.chooseNode(context)(loosePredicate)
      case sn => sn
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

    def intro (currentImpl : AGNode,
               absKind : NodeKind,
               absPolicy : AbstractionPolicy,
               wrongUsers : List[AGNode])
               (predicate : (AGNode) => Boolean =
                singleAbsIntroPredicate(currentImpl, absPolicy, absKind),
               context : => String =
               "Searching host for abstraction of %s ( %s, %s )".
                 format(currentImpl, absKind, absPolicy),
               parentsThatCanBeCreated : Int = 2) : Option[(AGNode, AbstractionPolicy)] ={

      graph.register {
        val abs = currentImpl.createAbstraction(absKind, absPolicy)
        findHost(abs, wrongUsers, context)(predicate) match {
          case None =>
            graph.transformations.undo()
            if(parentsThatCanBeCreated > 0)
              hostIntro(currentImpl, absKind, absPolicy, wrongUsers,
                parentsThatCanBeCreated - 1)
            else
              None
          case Some(host) =>
            host.content_+=(abs)
            Some(abs, absPolicy)
        }
      }
    }

    def hostIntro (toBeContained : AGNode,
                   absKind : NodeKind,
                   absPolicy : AbstractionPolicy,
                   wrongUsers : List[AGNode],
                   parentsThatCanBeCreated : Int) : Option[(AGNode, AbstractionPolicy)] = {
      println("\nhostIntro")
      toBeContained.container.kind.abstractKinds(absPolicy).find(_.canContain(absKind)) match {
        case None => throw new AGError("container abstraction creation error")
        case Some(cterAbsKind) =>
           intro(toBeContained.container,
            cterAbsKind, absPolicy, wrongUsers)({ n: AGNode => true},
            "Searching host for abstraction of %s's container:\n%s ( %s, %s )\n".
              format(toBeContained, toBeContained.container, cterAbsKind, absPolicy),
           parentsThatCanBeCreated ) match {
             case None => None
             case Some((cterAbs, _ )) => toBeContained.abstractions.find {
               case (existingAbs, `absPolicy`) => cterAbs.contains(existingAbs)
               case _ => false
             } match {
               case None =>
                 val newAbs = toBeContained.createAbstraction(absKind, absPolicy)
                 cterAbs content_+= newAbs
                 Some(newAbs, absPolicy)
               case existingAbsAndPolicy => existingAbsAndPolicy
             }
          }
        }
    }

    def aux(deg : Int, currentImpl : AGNode) : Option[(AGNode, AbstractionPolicy)] = {
      println("*** abs intro degree %d/%d ***".format(deg,degree))

      val (absKind, absPolicy) = getAbsKindAndPolicy(currentImpl)

      def doIntro(currentWrongUsers : List[AGNode]) =
        intro (currentImpl, absKind, absPolicy, currentWrongUsers)()

      if(deg == degree)
         doIntro(wrongUsers)
      else
         doIntro(List()) match {
          case None => throw new AGError("Single abs intro degree %d/%d error (currentImpl = %s)".
            format(deg, degree, currentImpl))
          case Some((abs, _)) =>
            aux(deg + 1, abs)
        }
    }

    graph.register {
      aux(1, impl) match {
        case None => None
        case Some((abs, absPolicy)) =>

          println("redirecting wrong users !!")
          wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
          /*if(impl.wrongUsers.nonEmpty){
            println("abs intro failure, remaing wrongusers :")
            print(impl.wrongUsers.mkString("-","\n-", "\n"))
            graph.transformations.undo()
            None
          }
          else{*/
            Some(abs)
          //}
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
                decisionMaker.modifyConstraints(LiteralNodeSet(wrongUsers), impl)
                /*if(impl.wrongUsers.nonEmpty)
                  throw new AGError ("cannot solve uses toward " + impl)*/
                ()
              case _ => ()
            }
        case _ => ()
      }
    }
  }

  var newCterNumGen = 0
  def solveContains(wronglyContained : AGNode) {
    graph.register{
      // detach for host searching : do not want to consider parent constraints
      val oldCter = wronglyContained.container
      wronglyContained.detach()

      val newCterOpt = findHost(wronglyContained, List(),
        "Moving " + wronglyContained + "\nSearching a host for it") {
        (potentialHost: AGNode) => !(potentialHost interloperOf wronglyContained)
      } match {
        case None =>
          val newCter = graph.nodeKinds.find(_.canContain(wronglyContained.kind)) match {
            case None =>
              throw new AGError("do not know how to create a valid parent for " + wronglyContained.kind)
            case Some(parentKind) =>
              newCterNumGen += 1
              graph.addNode("%s_container%d".format(wronglyContained.name, newCterNumGen), parentKind)
          }

          wronglyContained.addHideFromRootException(newCter)

          findHost(newCter, List(),
            "Moving " + wronglyContained + "\n" +
              newCter + " created to contain it.\n" +
              "Searching a host for " + newCter)(n => n != newCter) match {
            case None => None


            case Some(h) =>
              graph.root.content_-=(newCter)
              h.content_+=(newCter)
              Some(newCter)
          }
        case somehost => somehost
      }
      newCterOpt match {
        case Some(newCter) =>
          //re-attach before moving
          oldCter content_+= wronglyContained
          wronglyContained.moveTo(newCter)
        case None =>
          graph.transformations.undo()
          decisionMaker.modifyConstraints(LiteralNodeSet(wronglyContained.container), wronglyContained)
          if(wronglyContained.isWronglyContained)
            throw new SolvingError("Cannot solve %s contains violation".
              format(AGEdge.contains(wronglyContained.container, wronglyContained)))
      }
    }
  }

  def solveViolationsToward(target : AGNode){
    if(target.isWronglyContained)
      solveContains(target)
    if(target.wrongUsers.nonEmpty)
      solveUsesToward(target)
  }

  def solve(step : () => Unit) {

    def aux() {
      decisionMaker.violationTarget match {
        case None => ()
        case Some(target) =>
          solveViolationsToward(target)
          step()
          aux()
      }
    }
    aux()
  }

}
