package puck
package graph
package transformations

import puck.graph.transformations.rules._

class TransformationRules
( mergingCandidatesFinder : MergingCandidatesFinder,
  val intro : Intro,
  val rename : Renamer) {


  def findMergingCandidate = mergingCandidatesFinder.find _
  def mergeMatcherInstances = mergingCandidatesFinder.mergeMatcherInstances

  lazy val merger = new Merge(mergingCandidatesFinder)
  def mergeInto = merger.mergeInto _
  def removeConcreteNode = merger.removeConcreteNode _

  val redirection = Redirection
  val move = new Move(intro)

  def addHideFromRootException(g : DependencyGraph, node : NodeId, friend : NodeId): DependencyGraph =
    g.newGraph(nConstraints = g.constraints.addHideFromRootException(g, node, friend))
  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(g, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(g, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/

}
