package puck.graph.io

import puck.graph.{NodeId, DependencyGraph, DGNode}

trait DotHelper{
  def isDotSubgraph(k : DGNode) : Boolean
  def isDotClass(k: DGNode) : Boolean
  def fillColor(k: DGNode) : String
  def namePrefix(k: DGNode) : String
  def splitByKind(graph : DependencyGraph, ns: Seq[NodeId]) : Seq[Seq[NodeId]]
  //with java ((fields, Constructors, Methods), inner classes)
}
