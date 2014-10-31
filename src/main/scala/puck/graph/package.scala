package puck

/**
 * Created by lorilan on 30/10/14.
 */
package object graph {
/*   type NodeKind[Kind <: NodeKind[Kind]] = mutable.NodeKind[Kind]
   type NodeId[Kind <: NodeKind[Kind]] = mutable.AGNode[Kind]
   type AccessGraph[Kind <: NodeKind[Kind]] = mutable.AccessGraph[Kind]*/

  type NodeKind[Kind <: NodeKind[Kind]] = immutable.NodeKind[Kind]
  type AGNode[Kind <: NodeKind[Kind]] = immutable.AGNode[Kind]
  type NodeId[Kind <: NodeKind[Kind]] = immutable.AccessGraph.NodeId[Kind]
  type AGEdge[Kind <: NodeKind[Kind]] = immutable.AGEdge[Kind]
  val AGEdge = immutable.AGEdge
  type AccessGraph[Kind <: NodeKind[Kind]] = immutable.AccessGraph[Kind]

  type FilesHandler[Kind <: NodeKind[Kind]] = immutable.io.FilesHandler[Kind]

  type JavaNodeKind = javaAG.immutable.nodeKind.JavaNodeKind
  val JavaFilesHandler = javaAG.immutable.JavaFilesHandler
}
