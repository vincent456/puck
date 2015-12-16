package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{ConcreteNode, DGNode}

case class MergeAction
(controller : GraphController,
 consumed : DGNode,
 consumer : ConcreteNode)
  extends AbstractAction(s"Merge $consumed into this") {

  import controller.{graph, graphUtils}
  import graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller,"Merge action failure") {

      val sConsumerHost= graph.container(consumer.id)
      val tg =
        if(graph.container(consumed.id) != sConsumerHost) {
          val ma = new MoveAction(controller, graph.getConcreteNode(sConsumerHost.get), List(consumed.id))
          ma.doMove
        }
        else puck.graph.LoggedSuccess(graph.mileStone)

      tg flatMap (TR.merge.mergeInto(_, consumed.id, consumer.id))
    }

}
