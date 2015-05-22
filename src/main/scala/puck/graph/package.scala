package puck

import puck.graph.transformations.{Recordable}
import scalaz._

package object graph {

  type Try[+T] = PuckError \/ T
  type PuckFailure = NonEmptyList[PuckError]

  type NodePredicateT = (DependencyGraph, ConcreteNode) => Boolean

  type NodeId = Int

  type Recording = Seq[Recordable]
  val Recording = transformations.Recording

  type JavaNodeKind = javaGraph.nodeKind.JavaNodeKind
  val JavaNode = javaGraph.JavaDotHelper

  type FilesHandler = io.FilesHandler

  type ResultT = (DependencyGraph, Recording)

  type Mutability = Boolean

  def graphOfResult(result : ResultT) : DependencyGraph = result._1
  def recordOfResult(result : ResultT) : Recording = result._2
}
