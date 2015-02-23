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

  type NodeId = DependencyGraph.NodeId

  type JavaNodeKind = javaAG.nodeKind.JavaNodeKind
  val JavaFilesHandler = javaAG.JavaFilesHandler
  val JavaNode = javaAG.JavaNode
  val JavaSolver = javaAG.JavaSolver

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
