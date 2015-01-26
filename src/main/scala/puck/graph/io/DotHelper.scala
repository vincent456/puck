package puck.graph.io

import puck.graph.{DependencyGraph, NodeKind, NodeId}
/**
 * Created by lorilan on 07/05/14.
 */

trait DotHelper{
  def isDotSubgraph(k : NodeKind) : Boolean
  def isDotClass(k: NodeKind) : Boolean
  def fillColor(k: NodeKind) : String
  def namePrefix(k: NodeKind) : String
  def splitDotClassContent(graph : DependencyGraph, n: NodeId, visibility : VisibilitySet) :
  (Iterable[NodeId], Iterable[NodeId], Iterable[NodeId] , Iterable[NodeId])
  //with java ((fields, Constructors, Methods), inner classes)
}
