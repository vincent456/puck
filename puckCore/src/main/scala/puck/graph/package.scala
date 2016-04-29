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

import puck.graph.transformations.Recordable
import puck.util.{LoggedEither, Logged}

import scalaz._, Scalaz._

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

  type TypedNode = (ConcreteNode, Type)


  type Recording = Seq[Recordable]
  val Recording = transformations.Recording

  type Mutability = Boolean
  val isMutable = true
  val notMutable = false

  type SResult = (DependencyGraph, Int)

  def graphOfResult(res : Error \/ SResult) : DependencyGraph = res match {
    case \/-((g,_)) => g
    case _ => error("no graph in this result")
  }

  def graphOfResult(res : SResult) : DependencyGraph = res._1
  def recordOfResult(res : SResult) : Recording = res._1.recording


  implicit class GOps(val g : DependencyGraph) extends AnyVal{
    def logComment(msg : String) : LoggedG =
      g.comment(msg) set (msg + "\n")

    def toLoggedTG : LoggedTG =
      g.set("").toLoggedEither

  }

  implicit class LoggedOps[A](val lg: Logged[A]) extends AnyVal {
    def toLoggedEither[E] : LoggedEither[E, A] = LoggedEither(lg.written, lg.value.right[E])
    def toLoggedTry : LoggedTry[A] = toLoggedEither[Error]
  }

  implicit class LoggedOrOps[E, A](val lg: LoggedEither[E, A]) extends AnyVal {
    def error(e : E) : LoggedEither[E, A] = lg.left_>>(e)

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
