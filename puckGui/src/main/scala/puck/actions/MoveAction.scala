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


import puck.graph._
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, CreateVarStrategy}
import puck.gui.Log
import puck.util.LoggedEither._

import scala.swing.{Action, Publisher, Dialog}
import scala.swing.Swing.EmptyIcon
import scalaz.Scalaz._

object MoveAction {
  def getChoice(k : Seq[NodeKind]): Option[CreateVarStrategy] = {
    val choices = CreateParameter +: (k map CreateTypeMember.apply)

    Dialog.showInput(null, "Parameter or Field ?", "How to get self reference",
      Dialog.Message.Plain,
      icon = EmptyIcon, choices, choices.head)

  }

  def label(graph : DependencyGraph, ids : List[NodeId], newHost : DGNode) : String = {
    val movedStr = ids match {
      case List(id) => graph.getConcreteNode(id).name
      case Nil => sys.error("non empty list expected")
      case _ => "selected nodes"
    }
    s"Move $movedStr into ${newHost.name(graph)}"
  }

}

class MoveAction
(publisher : Publisher,
 newHost : DGNode,
 moved : List[NodeId])
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends Action(MoveAction.label(graph, moved, newHost)){

  import graphUtils.nodeKindKnowledge.kindOfKindType
  import graphUtils.{Rules => TR}

  def doMove : LoggedTG = {
    val g = graph.mileStone
    g.kindType(moved.head) match {
      case TypeDecl
           | StaticValueDecl
           | NameSpace =>
        moved.foldLoggedEither(g) {
          (g, id) => TR.move.staticDecl(g, id, newHost.id)
        }

      case InstanceValueDecl =>
        publisher.publish(Log(
          "/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\"))

        val oldContainer = g.container_!(moved.head)

        val isPullUp = g.isa_*(oldContainer, newHost.id)
        val isPushDown = g.isa_*(newHost.id, oldContainer)

        lazy val needNewReceiver = moved.exists {
          nid =>
            g.structuredType(nid) match{
              case Some(typ) => !(typ uses newHost.id)
              case None => sys.error("should have some type")
            }

        }

        if(isPullUp && isPushDown){
          error("new host and old host should be different")
        }
        else if(isPullUp)
          TR.move.pullUp(g,moved, oldContainer, newHost.id)
        else if(isPushDown)
          TR.move.pushDown(g,moved, oldContainer, newHost.id)
        else {

          val choice =
            if (!isPullUp && !isPushDown && needNewReceiver) {
              Some(MoveAction.getChoice(kindOfKindType(InstanceValueDecl)).
                getOrElse(CreateTypeMember(kindOfKindType(InstanceValueDecl).head)))
            }
            else None

          TR.move.typeMemberBetweenUnrelatedTypeDecl(g, moved, oldContainer, newHost.id, choice)
        }
      case kt =>
        LoggedError(s"move of $kt not implemented")
    }
  }

  override def apply() : Unit =
    printErrOrPushGraph(publisher, "Abstraction creation failure" )( doMove )


}
