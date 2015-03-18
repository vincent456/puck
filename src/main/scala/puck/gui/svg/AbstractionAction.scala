package puck.gui.svg

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph.{NodeId, NodeKind, ConcreteNode}
import puck.graph.constraints.AbstractionPolicy

import scalaz.{Success, Failure}

/**
  * Created by lorilan on 3/16/15.
  */


case class AbstractionAction(
  node : ConcreteNode,
  policy : AbstractionPolicy,
  kind : NodeKind,
  controller : SVGController)
  extends AbstractAction(s"$kind ($policy)"){

     def graph = controller.getGraph
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
         controller.transfoRules.createAbstraction(graph, node, kind, policy) match {
           case Failure(errs) =>
             controller.console.appendText("Abstraction creation failure\n" )
             errs.foreach(e => controller.console.appendText(e.getMessage + "\n"))
           case Success((abs, g)) =>

             val h = getHost(abs.kind)
             val g2 = g.addContains(h, abs.id)

             controller.pushGraph(g2)
         }

   }
