package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing._

import puck.PuckError
import puck.graph.constraints.AbstractionPolicy
import puck.graph._
import puck.gui.svg.SVGController

import scala.swing.Dialog
import scala.swing.Dialog.{Result, Message, Options}
import scalaz.-\/
import ShowDG._

object NodeCheckBox {
  def apply(graph : DependencyGraph,
            node : ConcreteNode) : NodeCheckBox =
    NodeCheckBox(graph, node, selected = true)

  def apply(graph : DependencyGraph,
            node : ConcreteNode,
            selected : Boolean) : NodeCheckBox =
    new NodeCheckBox(node, (graph, node).shows, selected)
}

class NodeCheckBox(val node : ConcreteNode, name : String, selected : Boolean)
  extends JCheckBox(name, selected)

class AbstractionAction(
  node : ConcreteNode,
  policy : AbstractionPolicy,
  kind : NodeKind,
  controller : SVGController)
  extends AbstractAction(s"$kind ($policy)"){

     import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}

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
             case None =>
               error(s"no available container for an abstraction of kind $absKind")
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

         val tAbsG : LoggedTry[(Abstraction, DependencyGraph)] =
           node.kind.kindType match {
           case TypeDecl =>
             val typeMembers = graph.content(node.id).toList.
                map(graph.getConcreteNode).
                filter(TR.abstracter.canBeAbstracted(graph, _, node, policy))
             val ckBoxes = typeMembers.map(NodeCheckBox(graph, _))

            def selectedNodes = ckBoxes.filter(_.isSelected).map(_.node)

             methodDialog(ckBoxes) match {
               case Result.Ok =>
                 TR.abstracter.
                   abstractTypeDeclAndReplaceByAbstractionWherePossible(graph.mileStone,
                     node, kind, policy, selectedNodes)
               case Result.Cancel =>
                 LoggedError(new PuckError("Operation Canceled"))
             }

           case _ =>
             TR.abstracter.createAbstraction(graph.mileStone, node, kind, policy)

         }

         tAbsG  map { case (abs, g) =>
            val absNodes = abs.nodes.map(g.getConcreteNode)
            absNodes.foldLeft(g){
               (g,n) =>
                 val h = getHost(n.kind)
                 g.addContains(h, n.id)
            }
         }
     }

}
