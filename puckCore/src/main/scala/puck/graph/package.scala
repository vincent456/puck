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

package puck

import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search.AutomataState
import puck.graph.transformations.Recordable
import puck.util.{Logged, LoggedEither, Logger}

import scalaz._
import Scalaz._

package object graph {

  type Try[+T] = PuckError \/ T


  type Error = PuckError

  def error(str : String) : Nothing = puck.error(str)
  def error() : Nothing = puck.error()

  type LoggedG = Logged[DependencyGraph]
  type LoggedTry[+A] = LoggedEither[Error, A]
  type LoggedTG = LoggedTry[DependencyGraph]

  type NodePredicate = (DependencyGraph, DGNode) => Boolean

  type NodeId = Int

  type NodeIdP = (NodeId, NodeId)

  type Parameterization = Map[NodeId, Type]
  type ParameterizationId = Int


  implicit class NodeIdPOps( val p : NodeIdP) extends AnyVal{

    def user = p._1
    def used = p._2

    def source = p._1
    def target = p._2

    def selfUse = p._1 == p._2

    def changeTarget(g : DependencyGraph, k : EKind, newTgt : NodeId) =
      g.changeTarget(k(p), newTgt)

    def splitWithTargets(g :  DependencyGraph, rTgt : NodeId, wTgt : NodeId) =
      g.splitWithTargets(Uses(p._1, p._2), rTgt, wTgt)
  }

  implicit class ConstraintsOps(val gc : (DependencyGraph, ConstraintsMaps)) extends AnyVal {
    def graph = gc._1
    def constraints = gc._2

    def violations() : Seq[DGEdge] = {
      (graph.containsList filter isViolation map Contains.apply) ++:
        (graph.usesList filter isViolation map Uses.apply)
    }

    def isViolation(e : NodeIdP) : Boolean = {
      val (source, target) = e
      constraints.isViolation(graph, source, target)
    }

    def isViolation(e : DGEdge) : Boolean = {
      e.kind match {
        case AbstractEdgeKind => false
        case _ => /*Contains | ContainsDef | ContainsParam | Uses | Isa => */
          constraints.isViolation(graph, e.source, e.target)

      }
    }

    def wrongUsers(id : NodeId) : List[NodeId] = constraints.wrongUsers(graph, id)
    def interloperOf(id1 : NodeId, id2 :NodeId) = constraints.isViolation(graph, id1, id2)
    def isWronglyUsed(id : NodeId) = constraints.wrongUsers(graph, id).nonEmpty
    def isWronglyContained(id : NodeId) : Boolean = constraints.isWronglyContained(graph, id)

    def printConstraints[V](logger : Logger[V], v : V) : Unit =
      constraints.printConstraints(graph, logger)(v)

  }

  type TypedNode = (ConcreteNode, Type)

  type Recording = Seq[Recordable]
  val Recording = transformations.Recording

  type Mutability = Boolean
  val isMutable = true
  val notMutable = false

  type DecoratedGraph[T] = (DependencyGraph, T)
  type SResult = DecoratedGraph[Option[(ConcreteNode, AutomataState)]]



  implicit class DecoratedGraphOps[T](val dg : DecoratedGraph[T]) extends  AnyVal {
    def graph : DependencyGraph = dg._1
    def decoration : T = dg._2
  }

  implicit class GOps(val g : DependencyGraph) extends AnyVal{
    def logComment(msg : String) : LoggedTG =
      LoggedSuccess(msg, g.comment(msg))
  }

  implicit class LoggedOps[A](val lg: Logged[A]) extends AnyVal {
    def toLoggedEither[E] : LoggedEither[E, A] = LoggedEither(lg.written, lg.value.right[E])
    def toLoggedTry : LoggedTry[A] = toLoggedEither[Error]
  }

  def LoggedError[A]( e: String): LoggedTry[A] =
    LoggedEither(e, -\/(new PuckError(e)))

  def LoggedError[A]( e: Error): LoggedTry[A] =
    LoggedEither("", -\/(e))

  def LoggedError[A](msg : String, e: PuckError): LoggedTry[A] =
    LoggedEither(msg, -\/(e))


  def LoggedSuccess[A]( a : A): LoggedTry[A] =
    LoggedSuccess("", a)

  def LoggedSuccess[A](msg : String, a : A): LoggedTry[A] =
    LoggedEither(msg, \/-(a))



}
