package puck.view.constraints

import java.awt.{Color, Dimension}

import puck.graph.DependencyGraph
import puck.graph.constraints.{Element, LiteralRangeSet, NamedRangeSet, Range, RangeSet, Scope}
import puck.view.util.{BubbleBorder, LabelImageHGlued}
import puck.view.{NodeKindIcons, _}

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked}
import puck.graph.ShowDG._
/**
  * Created by Loïc Girault on 12/15/16.
  */
class RangeSetPane
(graph : DependencyGraph,
 setNames : => Seq[String],
 namedSets : String => NamedRangeSet,
 title : String,
 var rangeSet: RangeSet,
 namedRangeSetAvailable : Boolean = true)
(implicit nodeKindIcons : NodeKindIcons)
  extends BorderPanel {

  class RangeLine(r : Range) extends BoxPanel(Orientation.Horizontal) {
    contents += new Label((graph,r).shows)
    contents += Swing.HGlue
    contents += new CheckBox("Root only") {
      r match {
        case Element(_)=> selected = true
        case _ => ()
      }
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          Swing onEDT {
            rangeSet = rangeSet - r + r.toggle
            init()
          }
      }
    }
    contents += buttonLabel(editimg){
      Swing onEDT {
        NodeSelector(graph, nodeKindIcons) foreach {
          nid =>
            rangeSet = rangeSet - r + Scope(nid)
            init()
        }
      }
    }
    contents += buttonLabel(deleteimg){
      rangeSet = rangeSet.setDef - r
      init()
    }

  }


  sealed abstract class Choice {
    def apply() : Unit
  }
  case object Named extends Choice {
    override val toString =  "Named set"
    def apply() = Swing onEDT {
      Dialog.showInput[String](
        message = "NamedSet or Literal Set ?",
        entries = setNames,
        initial = setNames.head) match {
        case None =>()
        case Some(n) =>
          rangeSet = namedSets(n)
          init()
      }
    }
  }
  case object Literal extends Choice{
    override val toString =  "Literal set"
    def apply() = Swing onEDT {
      NodeSelector(graph, nodeKindIcons) foreach {
        nid =>
          rangeSet = rangeSet + Scope(nid)
          init()
      }
    }

  }


  minimumSize = new Dimension(150, 100)
  border = new BubbleBorder(Color.BLACK, 1, 8)
  add(new LabelImageHGlued(title, addimg) {
    def action(mc: MouseClicked): Unit =
      rangeSet match {
        case NamedRangeSet(_, _) =>
          Dialog.showMessage(message = "Cannot add range to Named Range Set")
        case LiteralRangeSet(_) =>
            if(setNames.isEmpty || !namedRangeSetAvailable ) Literal()
            else Dialog.showInput[Choice](
              message = "NamedSet or Literal Set ?",
              entries = Seq(Named, Literal),
              initial = Named) match {
              case None => ()
              case Some(c) => c()
            }
      }
  }, Position.North)

  def init() : Unit = {
    val rangeSetPane =
      rangeSet match {
        case NamedRangeSet(setName, _) =>
          new BoxPanel(Orientation.Vertical) {
            contents += new BoxPanel(Orientation.Horizontal) {
              contents += new ComboBox(setNames) {
                selection.item = setName
              }
              contents += Swing.HGlue
              contents += buttonLabel(deleteimg) {
                rangeSet = LiteralRangeSet()
                init()
              }
            }
            contents += Swing.VGlue
          }


        case LiteralRangeSet(ranges) =>
          new ScrollPane {
            contents =
              new BoxPanel(Orientation.Vertical) {
                ranges foreach (r => contents += new RangeLine(r))
              }
          }
      }
    add(rangeSetPane, Position.Center)
    revalidate()
  }

  init()
}
