package puck.gui

import puck.graph.immutable.transformations.Transformation
import puck.graph._
import puck.search.SearchState

import scala.swing.{Button, Label, Orientation, BoxPanel}

/**
 * Created by lorilan on 22/10/14.
 */
class CSSearchStateComparator[Kind <: NodeKind[Kind], T](initialRecord : Seq[Transformation[Kind,T]],
                                                         sortedRes: Map[Int, Seq[SearchState[ResultT[Kind,T]]]])
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
    AccessGraph.areEquivalent(initialRecord,
      graphOfResult(cb1.selectedState.result),
      graphOfResult(cb2.selectedState.result))

  }
}
