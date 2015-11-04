package puck.intellij.concretize

import puck.graph._
import puck.graph.transformations.{Edge, Regular, Transformation}
import puck.intellij.PsiNodeWrapper

/**
 * Created by lorilan on 04/11/15.
 */
object CreateEdge {

  def unapply(t : Transformation) : Option[DGEdge] =
    t match {
      case Transformation(Regular, Edge(e)) => Some(e)
      case _ => None
    }

  def apply
  ( resultGraph : DependencyGraph,
    id2declMap: Map[NodeId, PsiNodeWrapper],
    Edge : DGEdge) : Unit = ()

}
