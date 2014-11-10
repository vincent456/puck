package puck

/**
 * Created by lorilan on 30/10/14.
 */
package object graph {
/*   type NodeKind[Kind <: NodeKind[Kind]] = mutable.NodeKind[Kind]
   type NodeId[Kind <: NodeKind[Kind]] = mutable.AGNode[Kind]
   type AccessGraph[Kind <: NodeKind[Kind]] = mutable.AccessGraph[Kind]*/


  type NodeKind = immutable.NodeKind
  type Type[T <: Type[ T]]= immutable.Type[T]
  type AGNode = immutable.AGNode
  type NodeId = immutable.AccessGraph.NodeId
  type AGEdge = immutable.AGEdge
  val AGEdge = immutable.AGEdge

  type AccessGraph = immutable.AccessGraph
  val AccessGraph = immutable.AccessGraph
  type GraphBuilder = immutable.GraphBuilder

  type JavaNodeKind = javaAG.immutable.nodeKind.JavaNodeKind
  val JavaFilesHandler = javaAG.JavaFilesHandler
  val JavaNode = javaAG.immutable.JavaNode
  val JavaSolver = javaAG.immutable.JavaSolver

  type ConstraintsParser = immutable.constraints.ConstraintsParser
  val ConstraintsParser = immutable.constraints.ConstraintsParser

  type Recording = immutable.transformations.Recording
  val Recording = immutable.transformations.Recording

  import scala.language.implicitConversions
  implicit def edgeToPair(edge : AGEdge) = (edge.source, edge.target)

  type FilesHandler = io.FilesHandler

  type ResultT = (AccessGraph, Recording)

  def graphOfResult(result : ResultT) : AccessGraph = result._1
  //in mutable version apply record before returning graph !
  def recordOfResult(result : ResultT) : Recording = result._2

}
