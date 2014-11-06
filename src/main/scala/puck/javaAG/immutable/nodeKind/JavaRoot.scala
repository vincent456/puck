package puck.javaAG.immutable.nodeKind

import puck.graph.immutable.AGRoot

/**
 * Created by lorilan on 31/07/14.
 */
case object JavaRoot extends JavaNodeKind with AGRoot{
  override val toString = "JavaRoot"
  /*override def canContain(k: JavaNodeKind) = k match {
    case Package(_) => true
    case _ => false
  }*/
}
