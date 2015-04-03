package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.ConcreteNode
import puck.gui.svg.SVGController

/**
 * Created by lorilan on 3/18/15.
 */
case class MergeAction(
  consumed : ConcreteNode,
  consumer : ConcreteNode,
  controller : SVGController)
  extends AbstractAction(s"Merge $consumed into this") {

  import controller.graph

  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Merge action failure") {
      controller.transfoRules.merge(graph, consumer.id, consumed.id)
    }

}
