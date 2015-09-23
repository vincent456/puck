package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{ConcreteNode, DGNode}
import puck.gui.svg.SVGController

case class MergeAction(
  consumed : DGNode,
  consumer : ConcreteNode,
  controller : SVGController)
  extends AbstractAction(s"Merge $consumed into this") {

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Merge action failure") {

      val sConsumerHost= graph.container(consumer.id)
      val tg =
        if(graph.container(consumed.id) != sConsumerHost) {
          val ma = new MoveAction(graph.getConcreteNode(sConsumerHost.get), List(consumed.id), controller)
          ma.doMove
        }
        else puck.graph.LoggedSuccess(graph.mileStone)

      tg flatMap (TR.merge.mergeInto(_, consumed.id, consumer.id))
    }

}
