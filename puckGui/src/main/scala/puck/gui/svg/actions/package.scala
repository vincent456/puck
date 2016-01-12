package puck.gui.svg
package actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.Uses
import puck.gui.EdgeForTypePrinting

import scala.swing.Publisher


class ShowTypeRelationshipTextualAction
( edge : Option[Uses],
  controller : SVGController)
extends AbstractAction(s"Show type bindings (text)"){
    def actionPerformed(e: ActionEvent) : Unit =
      edge foreach controller.printUseBindings
}

class ShowTypeRelationshipAction
( edge : Option[Uses],
  controller : Publisher)
extends AbstractAction(s"Show type bindings"){
    def actionPerformed(e: ActionEvent) : Unit =
      controller publish EdgeForTypePrinting(edge)
}
