package puck.javaGraph
package transformations

import puck.graph._
import puck.graph.transformations.MergeMatcherInstances
import puck.graph.transformations.rules.MergingCandidatesFinder
import nodeKind.Interface

object JavaTransformationHelper extends MergingCandidatesFinder {

  def mergeMatcherInstances : MergeMatcherInstances =
    JavaMergeMatcherInstances

  override def find(g : DependencyGraph, node : ConcreteNode) : Option[ConcreteNode] = {

    val nid = node.id
    node.kind match {
      case Interface if g.content(nid).nonEmpty =>
        g.concreteNodes.find { other =>
          node.canBeMergedInto(other, g) &&
            g.usersOf(nid).forall(!g.interloperOf(_,other.id)) &&
            g.usedBy(nid).forall(!g.interloperOf(other.id, _)
            )
        }
      case _ => None
    }

  }
  def findIn(g : DependencyGraph, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId] =
    InterfaceMergeMatcher.findMergingCandidateIn(g, method, interface)
}
