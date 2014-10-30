package puck.graph.mutable.io

import puck.graph.mutable.{AGNode, NodeKind}

/**
 * Created by lorilan on 07/05/14.
 */

trait DotHelper[Kind <: NodeKind[Kind]]{
  def isDotSubgraph(k : Kind) : Boolean
  def isDotClass(k: Kind) : Boolean
  def fillColor(k: Kind) : String
  def namePrefix(k: Kind) : String
  def splitDotClassContent(n: AGNode[Kind]) : (Iterable[AGNode[Kind]], Iterable[AGNode[Kind]], Iterable[AGNode[Kind]] , Iterable[AGNode[Kind]])
  //with java ((fields, Constructors, Methods), inner classes)
}
