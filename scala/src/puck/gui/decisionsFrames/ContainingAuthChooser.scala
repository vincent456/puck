package puck.gui.decisionsFrames

import puck.graph.AGNode
import puck.graph.constraints.{ElementConstraint, ScopeConstraint}
import scala.swing._

/**
 * Created by lorilan on 06/06/14.
 */
class ContainingAuthChooser(val container : AGNode, content : AGNode,
                            val violatedScopeConstraints : List[ScopeConstraint],
                            val violatedElementConstraints : List[ElementConstraint])
  extends DecisionFrame[Boolean]{

  title = "Grant containging authorization"

  contents = new BoxPanel(Orientation.Vertical) {

    val eltsStr = if(violatedElementConstraints.isEmpty) ""
    else violatedElementConstraints.mkString("\n","\n","\n")
    val scopeStr = if(violatedScopeConstraints.isEmpty) ""
    else violatedScopeConstraints.mkString("\n","\n","\n")

    contents += new TextArea(container + " is not supposed to contain " + content +
      "\nIt violates the following constraints :" + eltsStr + scopeStr){
      editable = false
    }

    contents += Button("OK"){
      ContainingAuthChooser.this.complete(false)
    }

  }

}
