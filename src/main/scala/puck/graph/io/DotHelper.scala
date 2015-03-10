package puck.graph.io

import puck.graph.{DGNode, DependencyGraph, NodeId}
/**
 * Created by lorilan on 07/05/14.
 */

trait DotHelper{
  def isDotSubgraph(k : DGNode) : Boolean
  def isDotClass(k: DGNode) : Boolean
  def fillColor(k: DGNode) : String
  def namePrefix(k: DGNode) : String
  def splitDotClassContent(graph : DependencyGraph, n: NodeId, visibility : VisibilitySet) :
  (Iterable[NodeId], Iterable[NodeId], Iterable[NodeId] , Iterable[NodeId])
  //with java ((fields, Constructors, Methods), inner classes)
}
