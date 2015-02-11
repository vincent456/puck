package puck.gui.search

import puck.graph._
import puck.graph.io.VisibilitySet
import puck.search.SearchState
import puck.util.{PuckFileLogger, PuckSystemLogger}

import scala.swing.{BoxPanel, Button, Label, Orientation}

/**
 * Created by lorilan on 22/10/14.
 */
class StateComparator
( initialRecord : Recording,
  sortedRes: Map[Int, Seq[SearchState[ResultT]]],
  printId : () => Boolean,
  printSig : () => Boolean,
  visibility : VisibilitySet)
  extends BoxPanel(Orientation.Vertical) {
  contents += new Label("Compare")
  val cb1 = new StateSelector(sortedRes, printId, printSig, visibility )
  val cb2 = new StateSelector(sortedRes, printId, printSig, visibility )

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
    val equivalent = DependencyGraph.areEquivalent(initialRecord,
      graphOfResult(cb1.selectedState.result),
      graphOfResult(cb2.selectedState.result), new PuckFileLogger(_ => true,
        new java.io.File("compare_log")))
      //new PuckSystemLogger(_ => true))

    println("equivalent = " + equivalent)

  }
}
