package puck.graph.immutable.io

import puck.graph.immutable.{AccessGraph, NodeKind}
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 07/05/14.
 */

trait DotHelper[Kind <: NodeKind[Kind]]{
  def isDotSubgraph(k : Kind) : Boolean
  def isDotClass(k: Kind) : Boolean
  def fillColor(k: Kind) : String
  def namePrefix(k: Kind) : String
  def splitDotClassContent(graph : AccessGraph[Kind], n: NodeId[Kind]) :
  (Iterable[NodeId[Kind]], Iterable[NodeId[Kind]], Iterable[NodeId[Kind]] , Iterable[NodeId[Kind]])
  //with java ((fields, Constructors, Methods), inner classes)
}
