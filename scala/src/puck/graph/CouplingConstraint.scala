package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */
abstract class CouplingConstraint
case class HideElement(val id:Int, val element: Int,
                       val interlopers: List[Int], val friends: List[Int]) extends CouplingConstraint
case class HideScope(val id:Int, val scope: Int, val facades : List[Int],
                     val interlopers: List[Int], val friends: List[Int]) extends CouplingConstraint
case class FriendOf(val friend:Int, val befriended: List[Int]) extends CouplingConstraint