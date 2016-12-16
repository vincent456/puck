package puck.view
package constraints


import javax.swing.{AbstractCellEditor, ImageIcon, JTable}
import javax.swing.table.{AbstractTableModel, TableCellEditor}

import puck.graph.DependencyGraph
import puck.graph.ShowDG._
import puck.graph.constraints._
import puck.view.util.{LabelImageHGlued, ResizeableOKCancelDialog}

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.event.{Event, MouseClicked}
import java.awt.{Component => AWTComponent}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.Dialog.Result
/**
  * Created by Loïc Girault on 12/13/16.
  */
object ConstraintsMapsDialog {
  def apply
  ( graph : DependencyGraph,
    cm : ConstraintsMaps )
  ( implicit nodeKindIcons : NodeKindIcons ) : Option[ConstraintsMaps] = {
    val cmp = new ConstraintsMapsPane(graph, cm)
    ResizeableOKCancelDialog(cmp) match {
      case Result.Ok => Some(cmp.constraintsMap)
      case _ => None
    }
  }
}
class ConstraintsMapsPane
( graph : DependencyGraph,
  cm : ConstraintsMaps )
( implicit nodeKindIcons : NodeKindIcons ) extends SplitPane {

  minimumSize = new Dimension(640,480)
  preferredSize = minimumSize
  val namedSets = mutable.Map[String, NamedRangeSet](cm.namedSets.toSeq:_*)

  val namedSetsPanel = new NamedSetsPanel(graph, namedSets)

  val hideConstraintsPanel =
    new ConstraintsPanel(graph, ArrayBuffer(cm.hideConstraints:_*), "Constraints",
      stringOfConstraint, ConstraintEditorDialog(graph, namedSets.keys.toSeq, cm.namedSets,_),
      canFriendBeEmpty = true)

  hideConstraintsPanel listenTo namedSetsPanel


  val friendConstraintsPanel =
    new ConstraintsPanel(graph, ArrayBuffer(cm.friendConstraints:_*), "Looseners",
      stringOfFriendConstraint, LoosenerEditorDialog(graph, namedSets.keys.toSeq, namedSets,_),
      canFriendBeEmpty = false)

  friendConstraintsPanel listenTo namedSetsPanel


  resizeWeight = 0.33
  enabled = false
  topComponent = new ScrollPane {
    contents = namedSetsPanel
  }
  bottomComponent = new SplitPane {
    resizeWeight = 0.5
    enabled = false
    topComponent = new ScrollPane{
      contents = hideConstraintsPanel
    }
    bottomComponent = new ScrollPane {
      contents = friendConstraintsPanel
    }
  }

  def constraintsMap : ConstraintsMaps =
    ConstraintsMaps(
      namedSets.toMap,
      friendConstraintsPanel.constraintMap,
      hideConstraintsPanel.constraintMap)


}

object EditLabel extends Label {
  icon = new ImageIcon(editimg)
}

case class NamedSetUpdate(oldName : String, newNamedRangeSet : NamedRangeSet) extends Event {
  def update(ct : Constraint) : Constraint =
    Constraint(
      update(ct.owners),
      update(ct.facades),
      update(ct.interlopers),
      update(ct.friends))

  def update(rs : RangeSet) : RangeSet = rs match {
    case NamedRangeSet(`oldName`, _) => newNamedRangeSet
    case NamedRangeSet(_, _) => rs
    case RootedRangeSet(rs0) => RootedRangeSet(update(rs0))
    case LiteralRangeSet(_) => rs
    case _ => throw new Error("RangeSet update : unhandled case")
  }
}
class NamedSetsPanel
( graph : DependencyGraph,
  namedSets : mutable.Map[String, NamedRangeSet] )
( implicit nodeKindIcons : NodeKindIcons
) extends BorderPanel {
  namedSetsPanel =>

  val mapKeys : ArrayBuffer[String] = ArrayBuffer[String](namedSets.keys.toSeq:_*)

  val tableModel = new AbstractTableModel {

    override def isCellEditable(row : Int, col : Int) : Boolean = col == 2

    def getRowCount: Int = mapKeys.length

    def getColumnCount: Int = 3

    def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      if(columnIndex == 0) mapKeys(rowIndex)
      else namedSets(mapKeys(rowIndex))
  }

  add(new LabelImageHGlued("Sets", addimg){
    def action(mc: MouseClicked): Unit = {
      NamedSetEditorDialog(graph,
        namedSets.keys.toSeq,
        namedSets, "") match {
        case None => ()
        case Some(newRs) =>
          if(newRs.id.nonEmpty){
            mapKeys.append(newRs.id)
            namedSets.update(newRs.id, newRs)
            tableModel.fireTableRowsUpdated(mapKeys.size - 2, mapKeys.size - 1)
          }
      }
    }

  }, Position.North)
  add(new Table{

    class NamedRangeSetRenderer extends Label {
      def prepare( rs : NamedRangeSet): Unit = {
        import puck.graph.ShowDG._
        text = (graph, rs.setDef).shows
      }
    }

    val namedRangedSetRenderer = new Table.AbstractRenderer[NamedRangeSet, NamedRangeSetRenderer](new NamedRangeSetRenderer) {
      def configure(t: Table, sel: Boolean, foc: Boolean, o: NamedRangeSet, row: Int, col: Int) = {
        //component variable is bound to your renderer
        component.prepare(o)
      }
    }

    val buttonEditor = new AbstractCellEditor with TableCellEditor {
      def getCellEditorValue: AnyRef = None
      def getTableCellEditorComponent(tab: JTable, value: AnyRef, isSelected: Boolean,
                                      row: Int, col: Int): AWTComponent = {
        Swing onEDT {
          val k = mapKeys(row)
          NamedSetEditorDialog(graph,
            namedSets.keys.toSeq,
            namedSets, k) match {
            case None => ()
            case Some(newRs) =>
              if(k == newRs.id)
                namedSets.update(k, newRs)
              else{
                namedSets.remove(k)
                mapKeys(row) = newRs.id
                namedSets.update(newRs.id, newRs)
              }

              tableModel.fireTableRowsUpdated(row, row)
              namedSetsPanel publish NamedSetUpdate(k, newRs)

          }
        }
        EditLabel.peer
      }
    }

    override def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component =
      column match {
        case 1 =>
        val v = model.getValueAt(
          peer.convertRowIndexToModel(row),
          peer.convertColumnIndexToModel(1)).asInstanceOf[NamedRangeSet]

        namedRangedSetRenderer.componentFor(this, isSelected, focused, v, row, 1)
        case 2 => EditLabel
        case _ => super.rendererComponent(isSelected, focused, row, column)
      }

    override def editor(row: Int, col: Int): TableCellEditor =
      if (col == 2) buttonEditor else super.editor(row, col)


    model = tableModel
    peer.getColumnModel.getColumn(2).setPreferredWidth(EditLabel.preferredSize.width)


  }, Position.Center)


}



class ConstraintsPanel
( graph : DependencyGraph,
  constraints : ArrayBuffer[Constraint],
  title : String,
  strBuilder : DGStringBuilder[Constraint],
  ctEditor : Constraint => Option[Constraint],
  canFriendBeEmpty : Boolean)
  extends BorderPanel {

  reactions += {
    case nsu @ NamedSetUpdate(_, _) =>
      for (idx <- constraints.indices) {
        val ct = constraints(idx)
        constraints.update(idx, nsu update ct)
      }

      tableModel.fireTableDataChanged()
  }

  val tableModel = new AbstractTableModel {

    override def isCellEditable(row : Int, col : Int) : Boolean = col == 1

    def getRowCount: Int = constraints.length

    def getColumnCount: Int = 2

    def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      constraints(rowIndex)
  }

  add(new LabelImageHGlued(title, addimg) {
    def action(mc: MouseClicked): Unit = {
      ctEditor(Constraint(RangeSet.empty, RangeSet.empty, RangeSet.empty, RangeSet.empty)) match {
        case None => ()
        case Some(ct) =>
          constraints.append(ct)
          tableModel.fireTableRowsInserted(constraints.size-2, constraints.size -1)
      }
    }

  }, Position.North)

  add(new Table{

    class ConstraintRenderer extends Label {
      def prepare( c : Constraint): Unit = {
        text = (graph, c).shows(strBuilder)
      }
    }

    val constraintRenderer = new Table.AbstractRenderer[Constraint, ConstraintRenderer](new ConstraintRenderer) {
      def configure(t: Table, sel: Boolean, foc: Boolean, o: Constraint, row: Int, col: Int) =
        component.prepare(o)
    }

    val buttonEditor = new AbstractCellEditor with TableCellEditor {
      def getCellEditorValue: AnyRef = None
      def getTableCellEditorComponent(tab: JTable, value: AnyRef, isSelected: Boolean,
                                      row: Int, col: Int): AWTComponent = {
        Swing onEDT {
          val ct = constraints(row)
          ctEditor(ct) match {
            case None => ()
            case Some(newCt) =>
              constraints(row) = newCt
          }
        }
        EditLabel.peer
      }
    }


    override def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component =
      if(column == 0) {
        val v = model.getValueAt(
          peer.convertRowIndexToModel(row),
          peer.convertColumnIndexToModel(column)).asInstanceOf[Constraint]

        constraintRenderer.componentFor(this, isSelected, focused, v, row, column)
      }
      else EditLabel


    override def editor(row: Int, col: Int): TableCellEditor =
      if (col == 1) buttonEditor else super.editor(row, col)

    model = tableModel
    peer.getColumnModel.getColumn(1).setPreferredWidth(EditLabel.preferredSize.width)
  }, Position.Center)

  def constraintMap =
    constraints.foldLeft(Map.empty[Range, ConstraintSet])(ConstraintsMaps.addConstraintToMap)
}