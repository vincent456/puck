package puck.view

import puck.control.{GraphStack, GraphChangeEvent}

import scala.swing._

/**
  * Created by Loïc Girault on 12/8/16.
  */
class UndoRedoPane
( bus : Publisher,
  graphStack: GraphStack
) extends BoxPanel(Orientation.Horizontal){

  contents +=
    new Button(new Action("Undo all") {

      enabled = false

      def apply() = graphStack.undoAll()

      reactions += {
        case _ : GraphChangeEvent =>
          enabled = graphStack.canUndo
      }
      listenTo(bus)
    })

  contents +=
    new Button(new Action("Undo") {

      enabled = false

      def apply() = graphStack.undo()

      reactions += {
        case _ : GraphChangeEvent =>
          enabled = graphStack.canUndo
      }
      listenTo(bus)
    })

  contents +=
    new Button(new Action("Redo") {

      enabled = false

      def apply() = graphStack.redo()

      reactions += {
        case _ : GraphChangeEvent =>
          enabled = graphStack.canRedo
      }
      listenTo(bus)
    })
}
