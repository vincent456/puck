package puck.javaGraph.nodeKind

import puck.graph.{AGRoot, NodeKind}

case object JavaRoot extends JavaNodeKind with AGRoot{
  override val toString = "JavaRoot"
  override def canContain(k: NodeKind) = k match {
    case Package => true
    case _ => false
  }
}
