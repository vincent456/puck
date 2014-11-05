package puck

/**
 * Created by lorilan on 30/10/14.
 */
package object graph {
/*   type NodeKind[Kind <: NodeKind[Kind]] = mutable.NodeKind[Kind]
   type NodeId[Kind <: NodeKind[Kind]] = mutable.AGNode[Kind]
   type AccessGraph[Kind <: NodeKind[Kind]] = mutable.AccessGraph[Kind]*/


  type NodeKind[Kind <: NodeKind[Kind]] = immutable.NodeKind[Kind]
  type Type[Kind <: NodeKind[Kind], T <: Type[Kind, T]]= immutable.Type[Kind, T]
  type AGNode[Kind <: NodeKind[Kind], T] = immutable.AGNode[Kind, T]
  type NodeId[Kind <: NodeKind[Kind]] = immutable.AccessGraph.NodeId[Kind]
  type AGEdge[Kind <: NodeKind[Kind]] = immutable.AGEdge[Kind]
  val AGEdge = immutable.AGEdge

  type AccessGraph[Kind <: NodeKind[Kind], T] = immutable.AccessGraph[Kind, T]
  val AccessGraph = immutable.AccessGraph
  type GraphBuilder[Kind <: NodeKind[Kind], T] = immutable.GraphBuilder[Kind, T]

  type JavaNodeKind = javaAG.immutable.nodeKind.JavaNodeKind
  val JavaFilesHandler = javaAG.JavaFilesHandler
  val JavaNode = javaAG.immutable.JavaNode
  val JavaSolver = javaAG.immutable.JavaSolver

  type ConstraintsParser[Kind <: NodeKind[Kind]] = immutable.constraints.ConstraintsParser[Kind]
  val ConstraintsParser = immutable.constraints.ConstraintsParser

  type Recording[Kind <: NodeKind[Kind], T] = immutable.transformations.Recording[Kind, T]


  import scala.language.implicitConversions
  implicit def edgeToPair[Kind <: NodeKind[Kind]] (edge : AGEdge[Kind]) = (edge.source, edge.target)

  type FilesHandler[Kind <: NodeKind[Kind], T] = io.FilesHandler[Kind, T]

  type ResultT[Kind <: NodeKind[Kind], T] = (AccessGraph[Kind,T], Recording[Kind, T])

  def graphOfResult[Kind <: NodeKind[Kind], T](result : ResultT[Kind, T]) : AccessGraph[Kind,T] = result._1
  //in mutable version apply record before returning graph !
  def recordOfResult[Kind <: NodeKind[Kind], T](result : ResultT[Kind, T]) : Recording[Kind,T] = result._2

}
