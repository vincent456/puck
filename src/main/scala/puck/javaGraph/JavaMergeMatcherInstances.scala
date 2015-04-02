package puck.javaGraph

import puck.graph.ConcreteNode
import puck.graph.transformations.{MergeMatcherInstances, MergeMatcher}
import puck.javaGraph.transformations.{FieldMergeMatcher, InterfaceMergeMatcher}
import nodeKind.Field
/**
 * Created by lorilan on 4/2/15.
 */
object JavaMergeMatcherInstances extends MergeMatcherInstances {

  def syntaxicMergeMatcher(n : ConcreteNode): MergeMatcher =
    n.kind match {
      case Field => new FieldMergeMatcher(n)
      case _ => semanticMergeMatcher(n)
    }

  def semanticMergeMatcher(n : ConcreteNode): MergeMatcher  =
    InterfaceMergeMatcher.mergeMatcher(n)

}
