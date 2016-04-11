/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

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
  absDefaultName : String,
  potentialsHost : Set[NodeId],
  abstractionChoices : List[ConcreteNode])
( implicit treeIcons : DGTreeIcons ) extends BorderPanel {

  val absNameTxtField = new TextField(absDefaultName)
  def absName : String = absNameTxtField.text

  val choices = abstractionChoices.map(NodeCheckBox(graph, _))
  def selectedNodes = choices.filter(_.selected).map(_.node)

  val treePane = new StaticDGTreePane(graph, potentialsHost, "Select host")
  def selectedHost = treePane.selecteNodes


  this.add(
    new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Abstraction's name :")
      contents += absNameTxtField
      contents += Swing.HGlue
    }, Position.North)

  this.add(new SplitPane(Orientation.Vertical) {
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
  }, Position.Center)
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

         val absDefaultName = TR.abstracter.abstractionName(graph, node, abskind, policy, None)

         val title = "Abstraction options"
         val potentialHosts = graph.nodes.filter(_.kind.canContain(abskind)).map(_.id).toSet
         val panel = new AbstractionPanel(graph, absDefaultName, potentialHosts, contentToAbstract)

         def setAbsName(arg : (Abstraction, DependencyGraph)) : (List[ConcreteNode], DependencyGraph) = {
           val (abs, g) = arg
           abs match {
             case AccessAbstraction(nid, _) if absDefaultName != panel.absName  =>
               val g1 = g.setName(nid, panel.absName)
               (List(g1.getConcreteNode(nid)), g1)
             case _ =>
              (abs.nodes map g.getConcreteNode, g)
           }
         }

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
               ltag map setAbsName map { case (absNodes, g) =>
                  absNodes.foldLeft(g){
                   (g, n) => g.addContains(panel.selectedHost.head, n.id)
                 }
               }
             }
         }

         confirm()
     }
}
