package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph._
import puck.gui.svg.SVGController
import puck.util.LoggedEither._


import scala.swing.Dialog
import scala.swing.Swing.EmptyIcon
import scalaz._, Scalaz._

object MoveAction {
  def getChoice(k : Seq[NodeKind]): Option[CreateVarStrategy] = {
    val choices = CreateParameter +: (k map CreateTypeMember.apply)

    Dialog.showInput(null, "Parameter or Field ?", "How to get self reference",
      Dialog.Message.Plain,
      icon = EmptyIcon, choices, choices.head)

  }

  def label(graph : DependencyGraph, ids : List[NodeId]) : String = {
    val movedStr = ids match {
      case List(id) => graph.getConcreteNode(id).name
      case Nil => sys.error("non empty list expected")
      case _ => "selected nodes"
    }
    s"Move $movedStr here"
  }

}

class MoveAction
( newHost : DGNode,
  moved : List[NodeId],
  controller : SVGController)
extends AbstractAction(MoveAction.label(controller.graph, moved)){

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}
  import graphUtils.nodeKindKnowledge.kindOfKindType
  override def actionPerformed(e: ActionEvent): Unit = {
    val g = graph.mileStone
    printErrOrPushGraph(controller, "Abstraction creation failure") {
      g.kindType(moved.head) match {
        case TypeDecl =>
          moved.foldLoggedEither(g) {
            (g, id) => TR.move.typeDecl(g, id, newHost.id)
          }

        case TypeMember =>
          controller.console.
            appendText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

          val host = g.getConcreteNode(g.container(moved.head).get)
          val uses = g.usesOfUsersOf(moved)
          val choice =
            if (TR.move.usedBySiblingsViaSelf(uses, g, host)) {
              Some(MoveAction.getChoice(kindOfKindType(TypeMember)).
                getOrElse(CreateTypeMember(kindOfKindType(TypeMember).head)))
            }
            else None

          TR.move.typeMember(g, moved, newHost.id, choice)(uses)
        case kt =>
          LoggedError(new PuckError(s"move of $kt not implemented"))
      }
    }
  }
}
