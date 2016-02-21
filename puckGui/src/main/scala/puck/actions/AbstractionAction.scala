package puck.actions

import java.awt.event.ActionEvent
import javax.swing._

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.AbstractionPolicy
import puck.gui.explorer.{StaticDGTreePane, DGTreeIcons}

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.Dialog.{Message, Options, Result}

object NodeCheckBox {
  def apply(graph : DependencyGraph,
            node : ConcreteNode) : NodeCheckBox =
    NodeCheckBox(graph, node, selected = true)

  def apply(graph : DependencyGraph,
            node : ConcreteNode,
            selected : Boolean) : NodeCheckBox =
    new NodeCheckBox(node, (graph, node).shows, selected)
}

class AbstractionPanel
( graph : DependencyGraph,
  potentialsHost : Set[NodeId],
  abstractionChoices : List[ConcreteNode])
(implicit treeIcons : DGTreeIcons)extends SplitPane(Orientation.Vertical) {

  val choices = abstractionChoices.map(NodeCheckBox(graph, _))
  def selectedNodes = choices.filter(_.selected).map(_.node)

  val treePane = new StaticDGTreePane(graph, potentialsHost, "Select host")
  def selectedHost = treePane.selecteNodes
  leftComponent = treePane

  if(abstractionChoices.isEmpty){
    dividerSize = 0
    resizeWeight = 1
  }
  else
    rightComponent = new BorderPanel {
      add(new Label("Select type member to abstract"), Position.North)
      add(new ScrollPane {
        contents = new BoxPanel(Orientation.Vertical) {
          choices foreach contents.+=
        }
        preferredSize = new Dimension(600, 250)


      }, Position.Center)
    }


}
class NodeCheckBox(val node : ConcreteNode, name : String, initiallySelected : Boolean)
  extends CheckBox(name){
  selected = initiallySelected
}

class AbstractionAction
(bus : Publisher,
 node : ConcreteNode,
 policy : AbstractionPolicy,
 abskind : NodeKind)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils,
 treeIcons: DGTreeIcons)
  extends AbstractAction(s"$abskind ($policy)"){

     import graphUtils.{transformationRules => TR}

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

     override def actionPerformed(e: ActionEvent): Unit =
       printErrOrPushGraph(bus,"Abstraction action failure") {

         val contentToAbstract : List[ConcreteNode] =
           node.kind.kindType match {
           case TypeDecl => graph.content(node.id).toList.
                map(graph.getConcreteNode).filter(n =>
               n.kind.kindType == InstanceValueDecl &&
               n.kind.abstractionNodeKinds(policy).nonEmpty).
                filter(TR.abstracter.canBeAbstracted(graph, _, node, policy))
           case _ => List()
         }

         val title = "Abstraction options"
         val potentialHosts = graph.nodes.filter(_.kind.canContain(abskind)).map(_.id).toSet
         val panel = new AbstractionPanel(graph, potentialHosts, contentToAbstract)

         def confirm() : LoggedTry[DependencyGraph] =
         Dialog.showConfirmation(null, panel.peer, title,
           Options.OkCancel, Message.Plain) match {
           case Result.Cancel
           | Result.Closed =>
             LoggedError("Operation Canceled")
           case Result.Ok =>
             if(panel.selectedHost.isEmpty) {
               Dialog.showMessage(null, "Select a host", messageType = Message.Error)
               confirm()
             }
             else if(panel.selectedHost.size > 1 ){
               Dialog.showMessage(null, "Select only one host", messageType = Message.Error)
               confirm()
             }
             else {
              val ltag : LoggedTry[(Abstraction, DependencyGraph)] =
                node.kind.kindType match {
                 case TypeDecl => TR.abstracter.
                   abstractTypeDeclAndReplaceByAbstractionWherePossible(graph.mileStone,
                     node, abskind, policy, panel.selectedNodes)
                 case _ =>
                   TR.abstracter.createAbstraction(graph.mileStone, node, abskind, policy)
               }
               ltag map { case (abs, g) =>
                 val absNodes = abs.nodes.map(g.getConcreteNode)
                 absNodes.foldLeft(g){
                   (g, n) => g.addContains(panel.selectedHost.head, n.id)
                 }
               }
             }
         }

         confirm()
     }
}
