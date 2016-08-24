/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph
package constraints.search
import puck.graph.GOps
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.{MutabilitySet, TransformationRules}
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, CreateVarStrategy}
import puck.util.LoggedEither
import puck.util.LoggedEither._

import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.{-\/, NonEmptyList, \/-}
import ShowDG._

/**
  * Created by Loïc Girault on 10/05/16.
  */

object SolvingActions {
  def absIntroPredicate
  ( impl : DGNode,
    absPolicy : AbstractionPolicy,
    absKind : NodeKind)
  ( implicit constraints: ConstraintsMaps,
    ms : MutabilitySet.T) : NodePredicate =


    (absKind.kindType, absPolicy) match {
      case (InstanceValueDecl, SupertypeAbstraction) =>
        (graph, potentialHost) => {
          val typeDecl = graph.container(impl.id).get
          val potentialSuperType = potentialHost.id
          val canExtends = !(graph, constraints).interloperOf(typeDecl, potentialSuperType)
          canExtends && graph.canContain(potentialHost, absKind)
        }
      case (_, SupertypeAbstraction) =>
        (graph, potentialHost) => !(graph, constraints).interloperOf(impl.id, potentialHost.id) &&
          graph.canContain(potentialHost, absKind)

      case (_, DelegationAbstraction) =>
        (graph, potentialHost) => !(graph, constraints).interloperOf(potentialHost.id, impl.id) &&
          graph.canContain(potentialHost, absKind)
    }
}
import SolvingActions.absIntroPredicate
class SolvingActions
(val rules : TransformationRules,
 implicit val constraints: ConstraintsMaps,
 implicit val ms : MutabilitySet.T) {

  var newCterNumGen = 0

  def containerKind
  (g : DependencyGraph,
   toBeContainedKind: NodeKind
  ): Stream[NodeKind] =
    g.nodeKinds.toStream.filter(_.canContain(toBeContainedKind))



  def introNodes(nodeKinds: Seq[NodeKind], g: DependencyGraph): Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    nodeKinds.toStream map {
      hostKind =>
        newCterNumGen += 1
        val hostName = s"extra${hostKind}Container$newCterNumGen"
        rules.intro(g, hostName, hostKind)
    } flatMap attribHost


  def hostIntro
  (toBeContained: ConcreteNode
  ): DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    g => containerKind(g, toBeContained.kind) map {
      hostKind =>
        newCterNumGen += 1
        val hostName = s"${toBeContained.name}_container$newCterNumGen"
        rules.intro(g, hostName, hostKind)
    } flatMap {
      case (toBeCtedHost, g) =>
        findHost(toBeCtedHost)(g) map (ltg => s"Searching host for $toBeCtedHost\n" <++: ltg map {
          case (hid, g1) => (toBeCtedHost.id, g1.addContains(hid, toBeCtedHost.id))
        })
    }


  def findHost
  (toBeContained: ConcreteNode): DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    chooseNode((dg, cn) => dg.canContain(cn, toBeContained) &&
      !(dg, constraints).isViolation(Contains(cn.id, toBeContained.id)) && {
      val dg1 : DependencyGraph =
        dg.container(toBeContained.id) map (dg.removeContains(_, toBeContained.id)) getOrElse dg
      val dg2 = dg1.addContains(cn.id, toBeContained.id)
      dg2.subTree(toBeContained.id).forall(!(dg2, constraints).isWronglyUsed(_)) //needed when moving violation host
    }
    )


  val attribHost : ((ConcreteNode, DependencyGraph)) => Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
    case (n, g) =>
      val hostStream = findHost(n)(g)
      hostStream  map (ltg => s"Searching host for $n\n" <++: ltg map {
        case (hid, g1) => (n.id, g1.addContains(hid, n.id))
      })
  }


  def canBeVirtualized : KindType => Boolean = {
    //case NameSpace => true
    case _ => false
  }

  def logNodeChosen(n : DGNode, graph : DependencyGraph) : LoggedTry[(NodeId, DependencyGraph)] =
    LoggedSuccess(s"node chosen is $n\n", (n.id, graph))


  def chooseNode
  ( predicate : NodePredicate)
  : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
    graph =>

      val choices = graph.concreteNodes.filter(predicate(graph,_)).toList

      if(choices.isEmpty) Stream(LoggedError(s"choose node, no choice"))
      else {

        val s = partitionByKind(graph)(choices, List()).toStream

        s flatMap {
          nel =>
            if (nel.tail.isEmpty) Stream(logNodeChosen(nel.head, graph))
            else if (canBeVirtualized(nel.head.kind.kindType)) {
              val (vn, g2) = graph.addVirtualNode(nel.map(_.id).toSet, nel.head.kind)
              Stream(logNodeChosen(vn, g2))
            }
            else {
              nel.toStream map (logNodeChosen(_, graph))
            }
        }
      }
  }

  def partitionByKind
  ( graph : DependencyGraph
  ) : (List[DGNode], List[NonEmptyList[DGNode]]) => List[NonEmptyList[DGNode]] = {
    case (Nil, acc) => acc
    case (hd :: tl, acc) =>
      val (same, diff) = tl.partition(n => n.kind == hd.kind)
      partitionByKind(graph)(diff, NonEmptyList[DGNode](hd, same:_*) +: acc)
  }



  def createVarStrategies
  ( g: DependencyGraph ): Stream[CreateVarStrategy] = {
    val tmKinds = g.nodeKindKnowledge.kindOfKindType(InstanceValueDecl)
    (CreateParameter +: (tmKinds map CreateTypeMember.apply)).toStream
  }


  def move
  ( wronglyContained : ConcreteNode
  ) : DependencyGraph => Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
    g0 =>
      val lg = s"trying to move $wronglyContained, searching host\n"
      findHost(wronglyContained)(g0) flatMap {
        case LoggedEither(log, -\/(err)) => Stream(LoggedEither(lg + log, -\/(err)))
        case LoggedEither(log, \/-((newCter, g))) =>
          val oldCter = g.container_!(wronglyContained.id)
          doMove(g, wronglyContained, oldCter, newCter) map (ltg => (lg + log) <++: ltg.map((newCter, _)))
      }
  }


  def doMove
  ( g: DependencyGraph,
    wronglyContained : ConcreteNode,
    oldCter : NodeId,
    newCter : NodeId
  ) :  Stream[LoggedTG] = {
    val stream: Stream[LoggedTG] = (wronglyContained.kind.kindType, g.styp(wronglyContained.id)) match {
      case (InstanceValueDecl, Some(typ)) =>

        val needNewReceiver = !(typ uses newCter)

        val isPullUp = g.isa_*(oldCter, newCter)
        val isPushDown = g.isa_*(newCter, oldCter)

        (isPullUp, isPushDown) match {
          case (true, true) =>
            assert(oldCter == newCter)
            Stream(LoggedSuccess(g))
          case (true, _) =>
            Stream(rules.move.pullUp(g, List(wronglyContained.id), oldCter, newCter))
          case (_, true) =>
            Stream(rules.move.pushDown(g, List(wronglyContained.id), oldCter, newCter))
          case _ if needNewReceiver =>
            createVarStrategies(g) map {
              cvs =>
                rules.move.typeMemberBetweenUnrelatedTypeDecl(g,
                  List(wronglyContained.id), oldCter, newCter, Some(cvs))
            }
          case _ =>
            Stream(rules.move.typeMemberBetweenUnrelatedTypeDecl(g, List(wronglyContained.id), oldCter, newCter, None))
        }

      case (TypeDecl, _)
           | (NameSpace, _)
           | (StaticValueDecl, _) =>
        Stream(rules.move.staticDecl(g, wronglyContained.id, newCter))

      case (TypeConstructor, _) =>
        Stream(LoggedError("TypeConstructor cannot be moved"))
      //true for java or C++ but not necessarily for all kind of languages
      //fail in move rule ? (which would rely on a canMove language specific predicate ?)

      case (kindType, styp) => Stream(LoggedError(s"doMove $kindType typed $styp not expected"))
    }

    stream map { ltg => s"Moving $wronglyContained " +
      s"from ${(g, oldCter).shows} to ${(g, newCter).shows}\n" <++: ltg }
  }

  def absIntro
  (impl : ConcreteNode
  ) : DependencyGraph => Stream[LoggedTry[(Abstraction, DependencyGraph)]] =
    g =>
      impl.kind.abstractionChoices.toStream map {
        case (absNodeKind, absPolicy) =>
          rules.abstracter.createAbstraction(g, impl, absNodeKind, absPolicy)
      } flatMap {
        case LoggedEither(log, -\/(err)) => Stream(LoggedEither(log, -\/(err)))
        case LoggedEither(log, \/-((abs, g2))) =>
          val absNodeKind = abs.kind(g2)

          (hostIntro(g2.getConcreteNode(abs.nodes.head))(g2) ++
            chooseNode(absIntroPredicate(impl,
              abs.policy, absNodeKind))(g2)).map {
            lt => lt.flatMap {
              case (host, g3) =>
                s"Searching host for $abs\n" <++: introAbsContainsAndIsa(abs, impl, g3, host)
            }
          }
      }

  def introAbsContainsAndIsa
  ( abs : Abstraction,
    impl : ConcreteNode,
    g : DependencyGraph,
    host : NodeId) : LoggedTry[(Abstraction, DependencyGraph)] = {
    val g2 = abs.nodes.foldLeft(g){_.addContains(host, _)}

    (g2.container(impl.id), g2.kindType(host)) match {
      case (None, _) => LoggedError("current impl has no container")
      case (Some(c), TypeDecl) =>
        if(c == host) LoggedError("node cannot be its own abstraction")
        else {
          val graph5 = g2.addAbstraction(c, AccessAbstraction(host, abs.policy))
          val graph6 =
            if (abs.policy == SupertypeAbstraction)
              graph5.addIsa(c, host).addUses(c, host)
            else graph5
          LoggedSuccess((abs, graph6))
        }
      case _ => LoggedSuccess((abs, g2))
    }

  }


  def redirectTowardExistingAbstractions
  ( used : ConcreteNode) :
  DependencyGraph => Stream[LoggedTG] =
    g =>
      redirectTowardExistingAbstractions(used,
        (g, constraints) wrongUsers used.id,
        g abstractions used.id)(g)





  def redirectTowardExistingAbstractions
  ( used : ConcreteNode,
    wrongUsers : List[NodeId],
    choices : Set[Abstraction]) :
  DependencyGraph => Stream[LoggedTG] =
    g =>
      if(wrongUsers.isEmpty) Stream(LoggedSuccess(g))
      else if(choices.isEmpty) Stream(LoggedError("No abstractions"))
      else {

        val cannotUseAbstraction: Abstraction => NodeId => Boolean = {
          abs => userId =>

            (abs,  g.getAccessKind((userId, used.id))) match {
              case (AccessAbstraction(absId, _), _) => (g, constraints).interloperOf(userId, absId)
              case (ReadWriteAbstraction(Some(rid), _), Some(Read)) => (g, constraints).interloperOf(userId, rid)
              case (ReadWriteAbstraction(_, Some(wid)), Some(Write)) => (g, constraints).interloperOf(userId, wid)
              case (ReadWriteAbstraction(Some(rid), Some(wid)), Some(RW)) =>
                (g, constraints).interloperOf(userId, rid) || (g, constraints).interloperOf(userId, wid)
              case _ => sys.error("should not happen")
            }

        }


        choices.toStream flatMap {
          abs =>
            val (remainingWus, wuToRedirect) =
              wrongUsers.partition(cannotUseAbstraction(abs))


            val ltg: LoggedTG =
              wuToRedirect.foldLoggedEither(g) {
                (g, wu) =>
                  // Uses(wu, used.id) can have be deleted in a
                  // previous iteration of this foldLoggedEither loop
                  if(g uses (wu, used.id))
                    rules.redirection.redirectUsesAndPropagate(g, (wu, used.id), abs)
                  else
                    LoggedSuccess(g)

              }

            ltg.value match {
              case \/-(g2) =>
                redirectTowardExistingAbstractions(used, remainingWus, choices - abs)(g2) map ( ltg.log <++: _)
              case -\/(_) => Stream(ltg)
            }
        }
      }
}