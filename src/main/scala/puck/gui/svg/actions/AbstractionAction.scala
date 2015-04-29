package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph.constraints.AbstractionPolicy
import puck.graph.{ConcreteNode, NodeId, NodeKind}
import puck.gui.svg.SVGController

/**
  * Created by lorilan on 3/16/15.
  */


class AbstractionAction(
  node : ConcreteNode,
  policy : AbstractionPolicy,
  kind : NodeKind,
  controller : SVGController)
  extends AbstractAction(s"$kind ($policy)"){

     def graph = controller.graph
     def getHost(absKind : NodeKind) : NodeId = {
       def aux(id : NodeId) : Option[NodeId] =
         graph.container(id).flatMap{ cterId =>
           val n = graph.getConcreteNode(cterId)
           if(n.kind.canContain(absKind))
             Some(cterId)
           else
             aux(cterId)
         }

       aux(node.id) match {
         case Some(id) => id
         case None =>
           graph.nodes.find(_.kind.canContain(absKind)) match {
             case Some(n) => n.id
             case None => throw new PuckError(s"no available container for $node")
           }
       }
     }

     override def actionPerformed(e: ActionEvent): Unit =
       printErrOrPushGraph(controller,"Abstraction action failure") {
         controller.transfoRules.createAbstraction(graph.mileStone, node, kind, policy).
           map { case (abs, g) =>
             val h = getHost(abs.kind)
             g.addContains(h, abs.id)
           }
       }

}
