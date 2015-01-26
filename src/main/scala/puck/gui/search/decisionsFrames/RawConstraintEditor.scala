package puck.gui.search.decisionsFrames

import puck.graph.constraints.Constraint
import puck.graph.{NamedNodeSet, DependencyGraph}

import scala.swing.{TextArea, Button, Orientation, BoxPanel}

import java.io.StringReader

/**
 * Created by lorilan on 11/06/14.
 */
class RawConstraintEditor( graph : DependencyGraph,
                           defs : List[NamedNodeSet],
                           constraints : List[Constraint],
                           finish : () => Unit)
  extends BoxPanel(Orientation.Vertical) {

  val console =/* new TextArea(defs.map(_.defString).mkString("\n") + "\n" + constraints.mkString("\n"))*/ ???
  contents += console
  /*contents += new BoxPanel(Orientation.Horizontal){
    contents += Button("OK"){
       defs.foreach(d => graph.nodeSets.remove(d.id))
       constraints.foreach{ct =>
         graph.constraints -= ct
         ct.owners.foreach(_.remove(ct))
       }
      new ConstraintsParser(graph)(new StringReader(console.text))
      finish()
    }
    contents += Button("Cancel"){
      finish()
    }
  }*/
}
