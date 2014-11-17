package puck.gui.search.decisionsFrames

import puck.graph.mutable.{AGNode, NodeKind}
import puck.graph.mutable.constraints.NodeSet

import scala.swing._

/**
 * Created by lorilan on 16/06/14.
 */
object NodeChooser{

  def apply[Kind <: NodeKind[Kind]](set : NodeSet[Kind], context : String) : Option[AGNode[Kind]] = DecisionFrame {
    () => new NodeChooser(set, context)
  }
}

class NodeChooser[Kind <: NodeKind[Kind]](nodes : NodeSet[Kind], context : String)
  extends DecisionFrame[Option[AGNode[Kind]]]{

  title = "Node selection"

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new TextArea(context)
    if(nodes.isEmpty){
      contents += new Label("No satisfying node found")
      contents += Button("OK"){
        NodeChooser.this.complete(None)
      }
    }
    else{
      val cb = new ComboBox(nodes.toSeq)
      contents += new FlowPanel(){
        contents += cb
      }
      contents += new FlowPanel(){
        contents += Button("Select none"){
          NodeChooser.this.complete(None)
        }
        contents += Button("OK"){
          NodeChooser.this.complete(Some(cb.selection.item))
        }
      }
    }
  }
}
