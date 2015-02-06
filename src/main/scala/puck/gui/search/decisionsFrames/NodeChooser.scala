package puck.gui.search.decisionsFrames


import puck.graph.{NodeSet, DGNode}

import scala.swing._

/**
 * Created by lorilan on 16/06/14.
 */
object NodeChooser{

  def apply(set : NodeSet, context : String) : Option[DGNode] = DecisionFrame {
    () => new NodeChooser(set, context)
  }
}

class NodeChooser(nodes : NodeSet, context : String)
  extends DecisionFrame[Option[DGNode]]{

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
          ???
          //NodeChooser.this.complete(Some(cb.selection.item))
        }
      }
    }
  }
}
