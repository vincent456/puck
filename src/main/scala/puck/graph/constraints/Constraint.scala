package puck.graph
package constraints

class Constraint(
  val owners : RangeSet,
  val facades : RangeSet,
  val interlopers : RangeSet,
  val friends : RangeSet){

  type GraphT = DependencyGraph
  
  val predicate : String = "hide"

  def addFriend(friend : Range) =
    new Constraint(owners, facades, interlopers, friends + friend)

  def isViolatedBy(graph : GraphT, edge : DGEdge): Boolean =
    owners.hasRangeThatContains_*(graph, edge.target) &&
      violated(graph, edge)

  def violated(graph : GraphT, edge : DGEdge): Boolean =
      interlopers.hasRangeThatContains_*(graph, edge.source) &&
      !friends.hasRangeThatContains_*(graph, edge.source) &&
      !facades.hasRangeThatContains_*(graph, edge.target)

}