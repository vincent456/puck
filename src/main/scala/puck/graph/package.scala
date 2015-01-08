package puck

/**
 * Created by lorilan on 30/10/14.
 */
package object graph {
/*   type NodeKind[Kind <: NodeKind[Kind]] = mutable.NodeKind[Kind]
   type NodeId[Kind <: NodeKind[Kind]] = mutable.AGNode[Kind]
   type AccessGraph[Kind <: NodeKind[Kind]] = mutable.AccessGraph[Kind]*/


  type NodeId = AccessGraph.NodeId

  type JavaNodeKind = javaAG.nodeKind.JavaNodeKind
  val JavaFilesHandler = javaAG.JavaFilesHandler
  val JavaNode = javaAG.JavaNode
  val JavaSolver = javaAG.JavaSolver

  type Recording = transformations.Recording
  val Recording = transformations.Recording

  import scala.language.implicitConversions
  implicit def edgeToPair(edge : AGEdge) : (NodeId, NodeId) = (edge.source, edge.target)

  type FilesHandler = io.FilesHandler

  type ResultT = (AccessGraph, Recording)

  def graphOfResult(result : ResultT) : AccessGraph = result._1
  //in mutable version apply record before returning graph !
  def recordOfResult(result : ResultT) : Recording = result._2

}
