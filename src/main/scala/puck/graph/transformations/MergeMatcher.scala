package puck.graph.transformations

import puck.graph.{DependencyGraph, ConcreteNode}

/**
 * Created by lorilan on 25/01/15.
 */

trait MergeMatcherInstances {
  def syntaxicMergeMatcher(n : ConcreteNode): MergeMatcher
  def semanticMergeMatcher(n : ConcreteNode): MergeMatcher
}

trait MergeMatcher {
  val node : ConcreteNode
  def canBeMergedInto(other : ConcreteNode, graph : DependencyGraph): Boolean = {
    other.kind == node.kind && other.id != node.id
  }
}
