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

package puck
package gui
package explorer

import java.awt.Color
import javax.swing.JTree

import puck.graph._
import puck.graph.constraints.ConstraintsMaps


import puck.gui.menus.{NodeMenu, ForbiddenDependencyTargetMenu}

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.event.{MouseClicked, Event}

case class FilterSource(filter : NodeId) extends Event
case class FilterTarget(filter : NodeId) extends Event


object ForbiddenDependenciesExplorer {
  def idToNameString(dg : DependencyGraph, nid : NodeId) : String =
    dg.getNode(nid) match {
      case n : ConcreteNode =>
        val name =
          if(n.kind.kindType == ValueDef)
            dg.container(n.id) map {
              idToNameString(dg, _)
            } getOrElse "OrphanDefinition"
          else n.name
        name + ShowDG.stringOfTypeOption(dg, dg.styp(n.id))
      case vn : VirtualNode => vn.name(dg)
    }
  def edgeToString(e:  DGEdge )(implicit graph : DependencyGraph): String = {
    val nodeName : NodeId => String = idToNameString(graph, _)

    if (e.source == e.target) s"${graph.getNode(e.source).kind} - ${nodeName(e.source)}"
    else {
      val (ksrc, ktgt) = (graph.getNode(e.source).kind, graph.getNode(e.target).kind)
      e.kind match {
        case AbstractEdgeKind =>
          val s = if (ksrc.toString endsWith "s") "es"
          else "s"
          if (ksrc == ktgt) s"$ksrc$s : ${nodeName(e.source)} -> ${nodeName(e.target)}"
          else s"$ksrc : ${nodeName(e.source)} -> $ktgt : ${nodeName(e.target)}"

        case _ =>
          s"${nodeName(e.source)} ($ksrc) - ${e.kind} -> ${nodeName(e.target)} ($ktgt)"
      }
    }
  }

}
import ForbiddenDependenciesExplorer._

class ForbiddenDependenciesExplorer
(control: PuckControl,
 allViolations : Seq[DGEdge],
 constraints : ConstraintsMaps)
(implicit graph : DependencyGraph,
 nodeKindIcons : NodeKindIcons)
  extends SplitPane {
  explorer =>
  import control.Bus

  this listenTo Bus

  def filterViolations
  ( sourceFilter : Option[NodeId],
    targetFilter : Option[NodeId]) : Seq[DGEdge] = {

    val vs = sourceFilter map (srcId =>
      allViolations.filter(e =>
        graph.contains_*(srcId, e.source)
      )) getOrElse allViolations

    targetFilter map (tgtId => vs.filter(e =>
      graph.contains_*(tgtId, e.target)
    )) getOrElse vs
  }

  import swing.Component.wrap
  def sources(violations : Seq[DGEdge]) : Set[NodeId] =
    violations.foldLeft(Set[NodeId]())((s,e) => s + e.source)
  def targets(violations : Seq[DGEdge]) : Set[NodeId] =
    violations.foldLeft(Set[NodeId]())((s,e) => s + e.target)


  trait ViolationTreeHandle {
    def extremities(violations : Seq[DGEdge]) : Set[NodeId]
    def exty(edge : DGEdge) : NodeId
    def event(nodeId : NodeId) : Event
  }

  class ViolationTree(handle : ViolationTreeHandle) extends
    JTree(TreeModelAdapter.subGraph(graph, handle.extremities(allViolations))) with DGTree {
    def icons : NodeKindIcons = nodeKindIcons

    override def convertNodeToText(n : DGNode) : String = {
      val vsCount = allViolations.count (e => graph.contains_*(n.id, handle.exty(e)))
      s"${n.name} ($vsCount)"
    }

    addNodeClickedAction(
      (e, n) =>
        if(isRightClick(e)) Swing.onEDT {
          val menu = if( explorer.constraints.isWronglyUsed(explorer.graph, n.id))
            ForbiddenDependencyTargetMenu(control, explorer.graph,
              n.asInstanceOf[ConcreteNode], nodeKindIcons)
            else
            NodeMenu(control, explorer.graph, n.id,
            List(), None)(nodeKindIcons, constraints)
          menu.contents += new Action("Node infos") {
            def apply() : Unit = Bus publish NodeClicked(n)
          }
          menu.show(Component wrap ViolationTree.this, e.getX, e.getY)
        } else {
          Bus publish handle.event(n.id)
          if(n.kind.kindType != NameSpace)
            Bus publish NodeClicked(n)
        }
    )
  }

  val sourceTree = new ViolationTree(new ViolationTreeHandle {
    def exty(edge: DGEdge): NodeId = edge.source

    def extremities(violations: Seq[DGEdge]): Set[NodeId] = sources(violations)

    def event(nodeId: NodeId): Event = FilterSource(nodeId)
  })

  val targetTree = new ViolationTree(new ViolationTreeHandle {
    def exty(edge: DGEdge): NodeId = edge.target

    def extremities(violations: Seq[DGEdge]): Set[NodeId] = targets(violations)

    def event(nodeId: NodeId): Event = FilterTarget(nodeId)
  })

  def selection(jTree: JTree): Option[NodeId] =
    Option(jTree.getLastSelectedPathComponent.asInstanceOf[DGNode]) map (_.id)

  def selectedSource : Option[NodeId] = selection(sourceTree)
  def selectedTarget : Option[NodeId] = selection(targetTree)


  var sourceFilter0 : Option[NodeId] = None
  var targetFilter0 : Option[NodeId] = None
  var filteredViolations0 : Seq[DGEdge] = allViolations

  var focusedEdge : Option[DGEdge] = None

  def sourceFilter : Option[NodeId] = sourceFilter0
  def sourceFilter_=(sid : Option[NodeId]) = {
    focusedEdge = None
    sourceFilter0 = sid
    Swing.onEDT {
      filteredViolations0 = filterViolations(sourceFilter, targetFilter)
      targetTree.setModel(TreeModelAdapter.subGraph(graph, targets(filteredViolations0)))
      sourceLabel.text = sid map graph.fullName getOrElse ""
      updateViolationListPane()
    }
  }
  def targetFilter : Option[NodeId] = targetFilter0
  def targetFilter_=(sid : Option[NodeId]) = {
    focusedEdge = None
    targetFilter0 = sid
    Swing.onEDT {
      filteredViolations0 = filterViolations(sourceFilter, targetFilter)
      sourceTree.setModel(TreeModelAdapter.subGraph(graph, sources(filteredViolations0)))
      targetLabel.text = sid map graph.fullName getOrElse ""
      updateViolationListPane()
    }
  }

  val sourceLabel = new Label()
  val targetLabel = new Label()

  reactions += {
    case FilterSource(srcFilter) if srcFilter == graph.rootId =>
      sourceFilter = None

    case FilterSource(srcFilter) =>
      sourceFilter = Some(srcFilter)

    case FilterTarget(tgtFilter) if tgtFilter == graph.rootId =>
      targetFilter = Some(tgtFilter)

    case FilterTarget(tgtFilter) =>
      targetFilter = Some(tgtFilter)

    case GraphFocus(_, e) =>
      focusedEdge = Some(e)
  }

  resizeWeight = 0.4
  this.leftComponent = new SplitPane(Orientation.Vertical){
    resizeWeight = 0.5

    this.leftComponent = new BorderPanel {
      add( new Label("Sources"), Position.North)
      add( new ScrollPane{
        contents = wrap(sourceTree)
      }, Position.Center)
      add(sourceLabel, Position.South)

    }

    this.rightComponent = new BorderPanel {
      add( new Label("Targets"), Position.North)
      add( new ScrollPane{
        contents = wrap(targetTree)
      }, Position.Center)
      add(targetLabel, Position.South)
    }


  }

  val violationListPane = new BoxPanel(Orientation.Vertical)

  this.rightComponent = new ScrollPane() {
    contents = violationListPane
  }


  def updateViolationListPane() : Unit = {
    violationListPane.contents.clear()
    violationListPane.contents +=
      new Label(s"${filteredViolations0.size} / ${allViolations.size} violation" +
        (if(filteredViolations0.size > 1) "s" else ""))


    var focusedLabel : Option[Label] = None

    filteredViolations0.foreach {
      edge =>
        violationListPane.contents +=  new Label(edgeToString(edge)) {
          self : Label =>
          focusedEdge.foreach{
            fe => if (edge == fe)
              self.foreground = Color.BLUE
              focusedLabel = Some(self)
          }
          listenTo(mouse.clicks)
          reactions += {
            case mc @ MouseClicked(_,_,_,_,_) =>
              val evt = mc.peer
              if(isRightClick(evt)){
                val menu : PopupMenu =
                  ForbiddenDependencyTargetMenu(control, graph,
                    graph getConcreteNode edge.target, nodeKindIcons)

                menu.contents +=  new Action("Focus in graph explorer") {
                  def apply() : Unit = Bus publish GraphFocus(graph, edge)
                }

                Swing.onEDT(menu.show(this, evt.getX, evt.getY))
              }
              else if(evt.getClickCount > 1) {
                self.foreground = Color.BLUE
                focusedLabel foreach (_.foreground = Color.BLACK)
                focusedLabel = Some(self)
                Bus publish GraphFocus(graph, edge)
              }
          }
        }
    }
    violationListPane.revalidate()
    violationListPane.repaint()

  }
  updateViolationListPane()
}


