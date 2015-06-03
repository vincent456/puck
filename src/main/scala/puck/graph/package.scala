package puck

import puck.graph.transformations.Recordable
import scalaz._, Scalaz._

package object graph {

  type Try[+T] = PuckError \/ T


  type Logged[A] = Writer[String, A]
  type LoggedOr[E, A] = EitherT[Logged, E, A]

  type Error = PuckError
  //type Log = Logged[Unit]
  type LoggedG = Logged[DependencyGraph]
  type LoggedTry[A] = LoggedOr[Error, A]
  type LoggedTG = LoggedTry[DependencyGraph]

  implicit class GOps(val g : DependencyGraph) extends AnyVal{
    def logComment(msg : String) : LoggedG =
      g.comment(msg) set (msg + "\n")

    def toLoggedTG : LoggedTG =
      g.set("").toLoggedOr

  }

  implicit class LoggedOps[A](val lg: Logged[A]) extends AnyVal {
    def toLoggedOr[E] : LoggedOr[E, A] =
      EitherT.right[Logged, E, A](lg)
    def toLoggedTry : LoggedTry[A] = toLoggedOr[Error]
  }

  implicit class LoggedOrOps[E, A](val lg: LoggedOr[E, A]) extends AnyVal {
    def error(e : E) : LoggedOr[E, A] =
      lg.flatMapF[A](_ => e.left[A].set(""))
  }



  def LoggedError[A]( e: Error): LoggedTry[A] =
    LoggedError(e, "")

  def LoggedError[A]( e: PuckError, msg : String): LoggedTry[A] =
    EitherT.eitherT[Logged, Error,A](e.left[A].set(msg))

  def LoggedSuccess[A]( a : A): LoggedTry[A] =
    LoggedSuccess(a, "")

  def LoggedSuccess[A]( a : A, msg : String): LoggedTry[A] =
    EitherT.eitherT[Logged, Error, A](a.right[Error].set(msg))

  type NodePredicateT = (DependencyGraph, ConcreteNode) => Boolean

  type NodeId = Int

  type Recording = Seq[Recordable]
  val Recording = transformations.Recording

  type JavaNodeKind = javaGraph.nodeKind.JavaNodeKind
  val JavaNode = javaGraph.JavaDotHelper

  type FilesHandler = io.FilesHandler


  type Mutability = Boolean

//  type ResultT = (DependencyGraph, Recording)
//  def graphOfResult(result : ResultT) : DependencyGraph = result._1
//  def recordOfResult(result : ResultT) : Recording = result._2

  type ResultT = DependencyGraph

  def graphOfResult(result : ResultT) : DependencyGraph = result

  def recordOfResult(result : ResultT) : Recording = graphOfResult(result).recording

}
