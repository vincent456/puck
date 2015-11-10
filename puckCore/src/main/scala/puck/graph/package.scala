package puck

import puck.graph.transformations.Recordable
import puck.util.{LoggedEither, Logged}

import scalaz._, Scalaz._

package object graph {

  type Try[+T] = PuckError \/ T


  type Error = PuckError

  def error(str : String) = throw new PuckError(str)

  type LoggedG = Logged[DependencyGraph]
  type LoggedTry[A] = LoggedEither[Error, A]
  type LoggedTG = LoggedTry[DependencyGraph]

  type NodePredicateT = (DependencyGraph, ConcreteNode) => Boolean

  type NodeId = Int

  type NodeIdP = (NodeId, NodeId)

  implicit class NodeIdPOps( val p : NodeIdP) extends AnyVal{

    def user = p._1
    def used = p._2
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
