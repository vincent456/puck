package puck.view.constraints

import puck.graph.DependencyGraph
import puck.graph.constraints._
import puck.view._
import puck.view.util.ResizeableOKCancelDialog

import scala.swing.Dialog.Result
import scala.swing._


object ConstraintEditorDialog{
  type RangeName = String
  def apply
  ( graph : DependencyGraph,
    namedSets : Map[String, NamedRangeSet],
    constraint : Constraint )
  ( implicit nodeKindIcons : NodeKindIcons )  : Option[Constraint] = {
    val cep = new ConstraintEditorPane(graph, namedSets, constraint)
    ResizeableOKCancelDialog(cep) match {
      case Result.Ok => Some(cep.constraint)
      case _ => None
    }
  }
}

class ConstraintEditorPane
(graph : DependencyGraph,
 namedSets : Map[String, NamedRangeSet],
 constraintIn : Constraint)
(implicit nodeKindIcons : NodeKindIcons)
  extends BoxPanel (Orientation.Vertical){
  ctPane =>

  val setNames = namedSets.keys.toSeq

  val hideRangePane = new RangeSetPane(graph, setNames, namedSets, "hide", constraintIn.owners)
  val facadeRangePane = new RangeSetPane(graph, setNames, namedSets, "except", constraintIn.facades)
  val interlopersRangePane =  new RangeSetPane(graph, setNames, namedSets, "from (everything if empty)", constraintIn.interlopers)
  val friendsRangePane = new RangeSetPane(graph, setNames, namedSets, "but-not-from", constraintIn.friends)

  contents += hideRangePane
  contents += facadeRangePane
  contents += interlopersRangePane
  contents += friendsRangePane

  def constraint =
    Constraint(
      if(hideRangePane.rangeSet.isEmpty) constraintIn.owners
      else hideRangePane.rangeSet,

      facadeRangePane.rangeSet,

      if(interlopersRangePane.rangeSet.isEmpty)
        LiteralRangeSet(Scope(DependencyGraph.rootId))
      else interlopersRangePane.rangeSet,

      friendsRangePane.rangeSet)

}





