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

package puck.view.explorer

import java.awt.Color
import java.awt.event.MouseEvent

import puck.graph._
import ShowDG._
import puck.control.{Log, NodeClicked, PuckControl}
import puck.view.menus.EdgeMenu
import puck.util.PuckLog

import scala.swing._
import puck.view._



class NodeInfosPanel
(bus : Publisher,
 control : PuckControl)
(implicit treeIcons : NodeKindIcons)
  extends ScrollPane {

  this listenTo bus
  import control.graph

  def edgeClick(c : Component, mkEdge : NodeId => DGEdge)(evt : MouseEvent, n : DGNode) : Unit = {
    val edge = mkEdge(n.id)
    if(graph exists edge) {
      if (isRightClick(evt))
        Swing.onEDT(new EdgeMenu(control.Bus, edge,
          control.printingOptionsControl,
          blurrySelection = false,
          control.constraints,
          control.mutabilitySet,
          graph,
          control.graphUtils,
          control.nodeKindIcons).show(c, evt.getX, evt.getY))
      else edge match {
        case Uses(src,tgt) =>
          bus publish Log(NodeInfosPanel.useBindings(graph, (src, tgt)))
        case _ => ()
      }
    }
  }

  def labelOrTreePane(s : Set[NodeId],
                      labelTxt : String,
                      treePaneTitle : => String,
                      commonToolTip : String,
                      action : Option[(MouseEvent, DGNode) => Unit] = None) : Component =
    if(s.isEmpty)
      new Label(labelTxt) {
        background = Color.white
        tooltip = commonToolTip
      }
    else new StaticDGTreePane(graph, s, treePaneTitle, Some(commonToolTip)) {
      action foreach tree.addNodeClickedAction
    }


  reactions += {
    case NodeClicked(n) if n.id != DependencyGraph.rootId =>
      Swing.onEDT(update(n.id))

  }

  def update(nodeId : NodeId) : Unit =
    contents = new BoxPanel(Orientation.Vertical) {


      val node = graph.getConcreteNode(nodeId)

      def mkStringWithNames(nodes: Iterable[NodeId]): String = {
        nodes.toSeq.map { nid =>
          graph.fullName(nid) + (graph.styp(nid) map (t => " " + (graph, t).shows) getOrElse "")
        }.sorted.mkString("\n", "\n", "\n")
      }


      val typeWeight =
        node.kind.kindType match {
          case TypeDecl =>
            val m = Metrics.typeWeight(graph, nodeId)
            s"Type Weight = ${m.toString}\n"

          case _ => ""
        }

      val v : PuckLog.Verbosity = (PuckLog.NoSpecialContext, PuckLog.Info)
      contents += new Label(s"${node.kind} ${graph.fullName(node.id)} : " +
        (graph, graph.structuredType(node.id)).shows + s"(${node.id})")

      val (providers, clients) = Metrics.providersAndClients(graph, node.id)
      private val subtree = graph.subTree(node.id, includeRoot = true)

      val (outgoings, internals) = {
        val (outs, ints) =
          Metrics.outgoingAndInternalDependencies(graph, node.id, subtree)
        (outs.size, ints.size)
      }
      val incomings = Metrics.incomingDependencies(graph, node.id, subtree).size

      val coupling = Metrics.coupling0(providers.size, clients.size, internals, outgoings, incomings)
      val cohesion = Metrics.cohesion(internals, outgoings, incomings)

      def packages(ids: Set[NodeId]) =
        ids.foldLeft(Set[NodeId]())((s, id) =>
          graph.containerOfKindType(NameSpace, id) map (s + _) getOrElse s)

      contents +=
        new Label(s"Internal dependencies : $internals \n") {
          background = Color.white
          tooltip = "Number of uses edges with both extremities contained by this node"
        }.leftGlued

      contents += new Label(s"Outgoing dependencies : $outgoings \n") {
        background = Color.white
        tooltip = "Number of uses edges for which this node contains the user but not the used"
      }.leftGlued

      contents += new Label(s"Incoming dependencies : $incomings \n") {
        background = Color.white
        tooltip = "Number of uses edges for which this node contains the used but not the user"
      }.leftGlued

      contents += new Label(typeWeight) {
        background = Color.white
        tooltip = "Number of strongly connected component divided by the number of children of this type node"
      }.leftGlued

      val subTypes = graph.directSubTypesId(node.id)
      val superTypes = graph.directSuperTypesId(node.id)

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += labelOrTreePane(subTypes, "No subtype",
          "Subtype(s) :", "Direct subtype(s)",
          Some(edgeClick(this, IsaEdge(_, node.id))))

        contents += labelOrTreePane(superTypes, "No super type",
          s"Super type(s)", "Direct super type(s)",
          Some(edgeClick(this, IsaEdge(node.id, _))))

      }

      contents += new TextArea("Abstractions :" +
        (if (graph.abstractions(node.id).isEmpty) "none\n"
        else mkStringWithNames(graph.abstractions(node.id).flatMap(_.nodes)))) {
        tooltip = "Set of node's abstractions"
        editable = false
      }.leftGlued

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += labelOrTreePane(clients, "No client",
          s"Clients : ${clients.size} in ${packages(clients).size} package(s)",
          "Nodes of the same kinds which contain a node using a node contained by this one")

        contents += labelOrTreePane(providers, "No provider",
          s"Providers : ${providers.size} in ${packages(providers).size} package(s)",
          "Nodes of the same kinds which contain a node used by a node contained by this one")

      }

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label(f"Coupling = $coupling%.2f") {
          tooltip = "1 - |Clients U Providers| / |Internal + Outgoing + Incoming dependencies |"
        }
        contents += Swing.HGlue
        contents += new Label(f"Cohesion :  $cohesion%.2f") {
          tooltip = "|Internal dependencies| / |Internal + Outgoing + Incoming dependencies|"
        }
      }

      val users = graph usersOf node.id
      val used = graph usedBy node.id
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += labelOrTreePane(users, "No users",
          s"Users of ${node.name} (${users.size}): ", "",
          Some(edgeClick(this, Uses(_, node.id))))

        contents += labelOrTreePane(used, "No used node",
          s"Used by ${node.name} (${used.size}): ", "",
          Some(edgeClick(this, Uses(node.id, _))))

      }
    }
}

object NodeInfosPanel {

  def useBindings(graph : DependencyGraph, u : NodeIdP) : String = {
    def print(sb : StringBuilder, u : NodeIdP) : StringBuilder = {
      val ustr = (graph, u).shows
      graph.getNode(u.used).kind.kindType match {
        case TypeDecl =>

          sb append s"Type uses $ustr selected\n"
          val tmus = graph.typeMemberUsesOf(u)
          if (tmus.isEmpty)
            sb append "No type member uses associated"
          else
            sb.append(tmus.map { tmu => (graph, tmu).shows }.mkString("TM uses are :\n", "\n", "\n"))

        case InstanceValue =>
          sb append s"Type Member uses $ustr selected\n"

          val tus = graph.typeUsesOf(u)
          if (tus.isEmpty)
            sb append "No type uses associated"
          else
            sb.append(tus.map { tu => (graph, tu).shows }.mkString("type uses are :\n", "\n", "\n"))

        case _ => sb
        //logger writeln "unhandled kind of used node"
      }
    }
    val sb = new StringBuilder
    print(sb, u).toString()
//    graph.nodePlusDefAndParams(u.user).foreach {
//      userDef => print(sb, graph.getUsesEdge(userDef, u.used).get)
//    }
  }
}