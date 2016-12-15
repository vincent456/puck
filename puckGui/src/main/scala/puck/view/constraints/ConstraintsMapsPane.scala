package puck.view
package constraints


import javax.swing.{AbstractCellEditor, ImageIcon, JTable}
import javax.swing.table.{AbstractTableModel, TableCellEditor}

import puck.graph.DependencyGraph
import puck.graph.ShowDG._
import puck.graph.constraints.{Constraint, ConstraintsMaps, NamedRangeSet, RangeSet}
import puck.view.util.LabelImageHGlued

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.event.{MouseClicked, TableRowsAdded}
import java.awt.{Component => AWTComponent}

import scala.collection.mutable.ArrayBuffer
/**
  * Created by Loïc Girault on 12/13/16.
  */
class ConstraintsMapsPane
( graph : DependencyGraph,
  cm : ConstraintsMaps )
( implicit nodeKindIcons : NodeKindIcons ) extends SplitPane {

  val namedSetsPanel = new NamedSetsPanel(graph, cm.namedSets)
  val friendConstraintsPanel =
    new ConstraintsPanel(graph, ArrayBuffer(cm.friendConstraints:_*), "Looseners",
      stringOfFriendConstraint, LoosenerEditorDialog(graph, cm.namedSets,_),
      canFriendBeEmpty = false)
  val hideConstraintsPanel =
    new ConstraintsPanel(graph, ArrayBuffer(cm.hideConstraints:_*), "Constraints",
      stringOfConstraint, ConstraintEditorDialog(graph, cm.namedSets,_),
      canFriendBeEmpty = true)

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


}



class NamedSetsPanel
( graph : DependencyGraph,
  namedSets : Map[String, NamedRangeSet]
) extends BorderPanel {

  val tableModel = new AbstractTableModel {

    val keys : Array[String] = namedSets.keys.toArray

    def getRowCount: Int = keys.length

    def getColumnCount: Int = 2

    def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      if(columnIndex == 0) keys(rowIndex)
      else namedSets(keys(rowIndex))
  }

  add(new BoxPanel(Orientation.Horizontal) {
    contents += new Label("Sets")
    contents += new Label {
      icon = new ImageIcon(addimg)
      listenTo(mouse.clicks)
      reactions += {
        case mc@MouseClicked(_, _, _, _, _) => println("Add !")
      }
    }
  }, Position.North)
  add(new Table{
    self =>

    class NamedRangeSetRenderer extends Label {
      def prepare( rs : NamedRangeSet): Unit = {
        import puck.graph.ShowDG._
        text = (graph, rs.setDef).shows
      }
    }

    val tcr = new Table.AbstractRenderer[NamedRangeSet, NamedRangeSetRenderer](new NamedRangeSetRenderer) {
      def configure(t: Table, sel: Boolean, foc: Boolean, o: NamedRangeSet, row: Int, col: Int) = {
        //component variable is bound to your renderer
        component.prepare(o)
      }
    }

    override def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component =
      if(column == 1) {
        val v = model.getValueAt(
          peer.convertRowIndexToModel(row),
          peer.convertColumnIndexToModel(1)).asInstanceOf[NamedRangeSet]

        tcr.componentFor(this, isSelected, focused, v, row, 1)
      }
      else super.rendererComponent(isSelected, focused, row, column)


    model = tableModel

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

  val tableModel = new AbstractTableModel {

    override def isCellEditable(row : Int, col : Int) : Boolean = col == 1

    def getRowCount: Int = constraints.length

    def getColumnCount: Int = 2

    def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      constraints(rowIndex)
  }

  add(new LabelImageHGlued("Sets", addimg) {
    def action(mc: MouseClicked): Unit = {
      ctEditor(Constraint(RangeSet.empty, RangeSet.empty, RangeSet.empty, RangeSet.empty)) match {
        case None => ()
        case Some(ct) =>
          constraints.append(ct)
          tableModel.fireTableRowsInserted(constraints.size-2, constraints.size -1)
      }
    }

  }, Position.North)

  val table = new Table{
    self =>

    object EditLabel extends Label {
      icon = new ImageIcon(editimg)
    }

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

  }


  add(table, Position.Center)
}