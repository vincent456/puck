package puck.view.constraints

import puck.graph.DependencyGraph
import puck.graph.constraints.{LiteralRangeSet, NamedRangeSet}
import puck.view.NodeKindIcons
import puck.view.util.ResizeableOKCancelDialog

import scala.swing.BorderPanel.Position
import scala.swing.Dialog.Result
import scala.swing.{BorderPanel, TextField}

/**
  * Created by Loïc Girault on 12/16/16.
  */

object NamedSetEditorDialog {

  def apply
  ( graph : DependencyGraph,
    setNames : => Seq[String],
    namedSets : String => NamedRangeSet,
    setName : String )
  ( implicit nodeKindIcons : NodeKindIcons )  : Option[NamedRangeSet] = {
    val nsep = new NamedSetEditorPane(graph, setNames, namedSets, setName)
    ResizeableOKCancelDialog(nsep) match {
      case Result.Ok => Some(nsep.namedRangeSet)
      case _ => None
    }
  }
}


class NamedSetEditorPane
(graph : DependencyGraph,
 setNamesSeq : => Seq[String],
 namedSets : String => NamedRangeSet,
 var setName : String)
( implicit nodeKindIcons : NodeKindIcons )
  extends BorderPanel {


  val nameField = new TextField(setName)
  add(nameField, Position.North)

  val rangeSetEditor =
  new RangeSetPane(graph,
    setNamesSeq,
    namedSets,
    "",
    try namedSets(setName).setDef
    catch {
      case t : Throwable => LiteralRangeSet.empty
    },
    namedRangeSetAvailable = false)

  add(rangeSetEditor, Position.Center)

  def namedRangeSet =
    NamedRangeSet(nameField.text, rangeSetEditor.rangeSet.setDef)

}
