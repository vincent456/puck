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

package puck.gui.menus

import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JPopupMenu}

import puck.actions.{RedirectAction0, RemoveEdgeAction}
import puck.graph._
import puck.gui.svg.actions._
import puck.gui._

import scala.swing.Publisher


class EdgeMenu
( publisher : Publisher,
  edge : NodeIdP,
  printingOptionsControl: PrintingOptionsControl,
  blurrySelection : Boolean,
  implicit val graph: DependencyGraph,
  implicit val graphUtils: GraphUtils)
  extends JPopupMenu {


  val (source, target) = edge

  println("target" + graph.getConcreteNode(target))
  println("abstractions " +graph.abstractions(target))

  if(graph.isViolation(edge)){
    val targetNode = graph.getConcreteNode(target)
    //add(new ManualSolveAction(publisher, targetNode))
    add(new AutoSolveAction(publisher, targetNode, printingOptionsControl))
  }

  var isIsaEdge = false
  var isUseEdge = false

  if(graph.isa(source, target)) {
    isIsaEdge = true
    add(new RemoveEdgeAction(publisher, Isa(source, target)))
  }

  def addUsesActions(src : NodeId, tgt : NodeId) : Unit =
    graph.getUsesEdge(src, tgt) foreach {
      uses =>
        isUseEdge = true
        add(new AbstractAction(s"Show type bindings"){
          def actionPerformed(e: ActionEvent) : Unit =
            publisher publish EdgeForTypePrinting(Some(uses))
        })


        val abstractions = graph.abstractions(tgt)
        if(abstractions.nonEmpty)
          add(new RedirectAction0(publisher, uses, abstractions.toSeq))
    }



  if(blurrySelection)
    graph.nodePlusDefAndParams(source).foreach{
      userDef => addUsesActions(userDef, target)
    }
  else
    addUsesActions(source, target)


  val isConcreteEdge = isIsaEdge || isUseEdge


  if(!isConcreteEdge)
      this.addMenuItem("Focus"){
        _ =>
          publisher publish GraphFocus(graph, AbstractEdgeKind(edge))
      }



}
