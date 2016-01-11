package puck.gui.svg
package actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.Uses



class ShowTypeRelationshipTextualAction
( edge : Option[Uses],
  controller : SVGController)
    extends AbstractAction(s"Show type bindings (text)")
  {
    import controller.graphStack.graph
    def actionPerformed(e: ActionEvent) : Unit =
      edge.foreach {
        uses =>
          controller.printUseBindings(uses)
          graph.definitionOf(uses.user).foreach{
            userDef =>
              controller.printUseBindings(graph.getUsesEdge(userDef, uses.used).get)
          }
      }

  }

  class ShowTypeRelationshipGraphicAction
  (edge : Option[Uses],
   controller : SVGController)
    extends AbstractAction(s"Show type bindings (graphic)")
  {
    def actionPerformed(e: ActionEvent) : Unit =
      controller.printingOptionsControl.setSelectedEdgeForTypePrinting(edge)
  }
