package puck.gui.decisionsFrames

import scala.swing.{TextArea, Button, Orientation, BoxPanel}
import puck.graph.constraints.{Constraint, ConstraintsParser, NamedNodeSet}
import puck.graph.{NodeKind, AccessGraph}
import java.io.StringReader

/**
 * Created by lorilan on 11/06/14.
 */
class RawConstraintEditor[K <: NodeKind[K]]( graph : AccessGraph[K],
                           defs : List[NamedNodeSet[K]],
                           constraints : List[Constraint[K]],
                           finish : () => Unit)
  extends BoxPanel(Orientation.Vertical) {

  val console = new TextArea(defs.map(_.defString).mkString("\n") + "\n" + constraints.mkString("\n"))
  contents += console
  contents += new BoxPanel(Orientation.Horizontal){
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
  }
}
