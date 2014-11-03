package puck.gui

import puck.graph.{ResultT, NodeKind, recordOfResult}
import puck.search.SearchState

import scala.swing.{Button, Label, Orientation, BoxPanel}

/**
 * Created by lorilan on 22/10/14.
 */
class CSSearchStateComparator[Kind <: NodeKind[Kind], T](sortedRes: Map[Int, Seq[SearchState[ResultT[Kind,T], _]]])
  extends BoxPanel(Orientation.Vertical) {
  contents += new Label("Compare")
  val cb1 = new CSSearchStateComboBox(sortedRes)
  val cb2 = new CSSearchStateComboBox(sortedRes)

  this deafTo this

  this listenTo cb1
  this listenTo cb2

  reactions += {
    case e => //println("CSSearchStateComparator : chaining event")
      publish(e)
  }

  contents += cb1
  contents += new Label("and")
  contents += cb2
  contents += Button(">>") {
    val recording1 = recordOfResult(cb1.selectedState.result)
    val recording2 = recordOfResult(cb2.selectedState.result)
    recording1.produceSameGraph(recording2)
  }
}
