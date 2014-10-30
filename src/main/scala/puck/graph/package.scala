package puck

/**
 * Created by lorilan on 30/10/14.
 */
package object graph {
   type NodeKind[Kind <: NodeKind[Kind]] = mutable.NodeKind[Kind]
   type AccessGraph[Kind <: NodeKind[Kind]] = mutable.AccessGraph[Kind]
}
