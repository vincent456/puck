package puck

import puck.graph.transformations.Recordable
import scalaz._, Scalaz._

package object graph {

  type Try[+T] = PuckError \/ T


  type Logged[A] = Writer[String, A]
  type LoggedOr[E, A] = EitherT[Logged, E, A]

  implicit class LoggedOps[A](lg: Logged[A]) extends AnyVal {
    def toLoggedOr[E] : LoggedOr[E, A] =
      EitherT.right[Logged, E, A](lg)
  }

  implicit class LoggedOrOps[E, A](lg: LoggedOr[E, A]) extends AnyVal {
    def error(e : E) : LoggedOr[E, A] =
      lg.flatMapF[A](_ => e.left[A].set(""))
  }

  type LoggedG = Logged[DependencyGraph]
  type LoggedTG = LoggedOr[PuckError, DependencyGraph]


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
  def recordOfResult(result : ResultT) : Recording = result.recording

}
