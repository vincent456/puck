package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JMenuItem, JPopupMenu}

import puck.graph.{Isa, DGEdge}

/**
 * Created by lorilan on 3/31/15.
 */
class EdgeRightClickMenu
( private val controller : SVGController,
  edge : DGEdge)
  extends JPopupMenu {

  import controller.{getGraph, console}

  if(getGraph.isViolation(edge)){
    add(new JMenuItem(new AbstractAction("Solve"){
      override def actionPerformed(e: ActionEvent): Unit =
        console.setText("Solve !!!")
    }))
  }

  edge.kind match {
    case Isa =>
      add(new JMenuItem(new AbstractAction("Remove"){
        override def actionPerformed(e: ActionEvent): Unit =
          console.setText("Remove !!!")
      }))
    case _ => ()
  }

}
