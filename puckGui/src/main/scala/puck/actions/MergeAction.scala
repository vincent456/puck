package puck.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.graph.{GraphUtils, DependencyGraph, ConcreteNode, DGNode}

import scala.swing.Publisher

case class MergeAction
(controller : Publisher,
 consumed : DGNode,
 consumer : ConcreteNode)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
  extends AbstractAction(s"Merge $consumed into this") {

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
