package puck

import scalaz._

/**
 * Created by lorilan on 30/10/14.
 */
package object graph {
/*   type NodeKind[Kind <: NodeKind[Kind]] = mutable.NodeKind[Kind]
   type NodeId[Kind <: NodeKind[Kind]] = mutable.AGNode[Kind]
   type AccessGraph[Kind <: NodeKind[Kind]] = mutable.AccessGraph[Kind]*/


  type Try[T] = ValidationNel[PuckError, T]
  type PuckFailure = NonEmptyList[PuckError]

  type NodeId = DependencyGraph.NodeId

  type JavaNodeKind = javaGraph.nodeKind.JavaNodeKind
  val JavaFilesHandler = javaGraph.JavaFilesHandler
  val JavaNode = javaGraph.JavaDotHelper
  val JavaSolver = javaGraph.JavaSolver

  type Recording = graph.transformations.Recording
  val Recording = graph.transformations.Recording

  import scala.language.implicitConversions
  implicit def edgeToPair(edge : DGEdge) : (NodeId, NodeId) = (edge.source, edge.target)

  type FilesHandler = io.FilesHandler

  type ResultT = (DependencyGraph, Recording)

  def graphOfResult(result : ResultT) : DependencyGraph = result._1
  //in mutable version apply record before returning graph !
  def recordOfResult(result : ResultT) : Recording = result._2
}
