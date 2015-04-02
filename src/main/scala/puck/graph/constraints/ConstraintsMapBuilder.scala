package puck.graph.constraints

/**
 * Created by lorilan on 4/2/15.
 */
class ConstraintsMapBuilder {

  var constraintsMap = ConstraintsMaps()

  def setDefs(defs : Map[String, NamedRangeSet]): Unit = {
    constraintsMap = constraintsMap.copy(namedSets =  defs)
  }

  def addHideConstraint(owners : RangeSet,
                        facades : RangeSet,
                        interlopers : RangeSet,
                        friends : RangeSet) = {
    val ct = new Constraint(owners, facades, interlopers, friends)

    val hideConstraintsMap = owners.foldLeft(constraintsMap.hideConstraints){
      case (map, owner) =>
        val s = map.getOrElse(owner, new ConstraintSet())
        map + (owner -> (s + ct) )
    }
    constraintsMap = constraintsMap.copy(hideConstraints = hideConstraintsMap)
  }

  def addFriendConstraint( friends : RangeSet,
                           befriended : RangeSet) = {
    val ct = new Constraint(befriended, RangeSet.empty(), RangeSet.empty(), friends)

    val friendCtsMap = befriended.foldLeft(constraintsMap.friendConstraints){
      case (map, owner) =>
        val s = map.getOrElse(owner, new ConstraintSet())
        map + (owner -> (s + ct) )
    }

    constraintsMap = constraintsMap.copy(friendConstraints = friendCtsMap)
  }
}

