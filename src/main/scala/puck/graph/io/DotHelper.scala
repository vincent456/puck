package puck.graph.io

import puck.graph.{AccessGraph, NodeKind, NodeId}
/**
 * Created by lorilan on 07/05/14.
 */

trait DotHelper[Kind <: NodeKind[Kind]]{
  def isDotSubgraph(k : Kind) : Boolean
  def isDotClass(k: Kind) : Boolean
  def fillColor(k: Kind) : String
  def namePrefix(k: Kind) : String
  def splitDotClassContent(graph : AccessGraph[Kind,_], n: NodeId[Kind]) :
  (Iterable[NodeId[Kind]], Iterable[NodeId[Kind]], Iterable[NodeId[Kind]] , Iterable[NodeId[Kind]])
  //with java ((fields, Constructors, Methods), inner classes)
}
