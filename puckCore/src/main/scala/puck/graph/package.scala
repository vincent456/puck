package puck

import puck.graph.transformations.Recordable
import puck.util.LoggedEither
import puck.util.Logged

import scalaz._, Scalaz._

package object graph {

  type Try[+T] = PuckError \/ T


  type Error = PuckError

  def error(str : String) = throw new PuckError(str)

  type LoggedG = Logged[DependencyGraph]
  type LoggedTry[A] = LoggedEither[Error, A]
  type LoggedTG = LoggedTry[DependencyGraph]

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


  def LoggedError[A]( e: Error): LoggedTry[A] =
    LoggedError(e, "")

  def LoggedError[A]( e: PuckError, msg : String): LoggedTry[A] =
    LoggedEither(msg, -\/(e))


  def LoggedSuccess[A]( a : A): LoggedTry[A] =
    LoggedSuccess(a, "")

  def LoggedSuccess[A]( a : A, msg : String): LoggedTry[A] =
    LoggedEither(msg, \/-(a))

  type NodePredicateT = (DependencyGraph, ConcreteNode) => Boolean

  type NodeId = Int

  type NodeIdP = (NodeId, NodeId)

  type TypedNode = (ConcreteNode, Type)

  implicit class NodeIdPOps( val p : NodeIdP) extends AnyVal{

    def user = p._1
    def used = p._2
  }



  type Recording = Seq[Recordable]
  val Recording = transformations.Recording

  type Mutability = Boolean
  val mutable = true
  val notMutable = false

  type ResultT = DependencyGraph

  def graphOfResult(result : ResultT) : DependencyGraph = result

  def recordOfResult(result : ResultT) : Recording = graphOfResult(result).recording

}
