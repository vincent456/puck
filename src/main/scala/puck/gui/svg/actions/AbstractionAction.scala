package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing._

import puck.PuckError
import puck.graph.constraints.AbstractionPolicy
import puck.graph._, ShowDG._
import puck.gui.svg.SVGController

import scala.swing.Dialog
import scala.swing.Dialog.{Result, Message, Options}
import scalaz.-\/


object NodeCheckBox {
  def apply(graph : DependencyGraph,
            node : ConcreteNode) : NodeCheckBox =
    NodeCheckBox(graph, node, selected = true)

  def apply(graph : DependencyGraph,
            node : ConcreteNode,
            selected : Boolean) : NodeCheckBox =
    new NodeCheckBox(node, showDG[DGNode](graph).shows(node), selected)
}

class NodeCheckBox(val node : ConcreteNode, name : String, selected : Boolean)
  extends JCheckBox(name, selected)

class AbstractionAction(
  node : ConcreteNode,
  policy : AbstractionPolicy,
  kind : NodeKind,
  controller : SVGController)
  extends AbstractAction(s"$kind ($policy)"){

     import controller.{graph,transfoRules}

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

    private def methodDialog(choices : Seq[NodeCheckBox]) : Result.Value = {
      val message = "Select type member to abstract"
      val title = "TypeDecl abstraction options"
      val panel = new JPanel(){
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
        this add new JLabel(message)
        choices foreach this.add
      }
      Dialog.showConfirmation(null, panel, title, Options.OkCancel, Message.Plain)
    }

     override def actionPerformed(e: ActionEvent): Unit =
       printErrOrPushGraph(controller,"Abstraction action failure") {

         val tAbsG = graph.kindType(node) match {
           case TypeDecl =>
             val typeMembers = graph.content(node.id).toSeq.
                map(graph.getConcreteNode).
                filter(graph.kindType(_)==TypeMember)
             val ckBoxes = typeMembers.map(NodeCheckBox(graph, _))

            def selectedNodes = ckBoxes.filter(_.isSelected).map(_.node)

             methodDialog(ckBoxes) match {
               case Result.Ok =>
                 transfoRules.abstracter.
                   abstractTypeDeclAndReplaceByAbstractionWherePossible(graph.mileStone, node, kind, policy, selectedNodes)
               case Result.Cancel =>
                 -\/(new PuckError("Operation Canceled"))
             }

           case _ =>
             transfoRules.abstracter.createAbstraction(graph.mileStone, node, kind, policy)

         }

         tAbsG  map { case (abs, g) =>
           val h = getHost(abs.kind)
           g.addContains(h, abs.id)
         }
     }

}
