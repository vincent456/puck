package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph._
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.Field


import scala.swing.Dialog
import scala.swing.Swing.EmptyIcon
import scalaz.{\/-, -\/}

object MoveAction {
  def getChoice(k : NodeKind): Option[CreateVarStrategy] = {
    val choices = Seq(CreateTypeMember(k),
                      CreateParameter)

    Dialog.showInput(null, "Parameter or Field ?", "How to get self reference",
      Dialog.Message.Plain,
      icon = EmptyIcon, choices, choices.head)

  }
}

class MoveAction
( newHost : DGNode,
  moved : ConcreteNode,
  controller : SVGController)
extends AbstractAction(s"Move ${moved.name} here"){


  val move = controller.transfoRules.move

  override def actionPerformed(e: ActionEvent): Unit = {
    val graph = controller.graph.mileStone
    (graph.kindType(moved) match {
      case TypeDecl =>
        move.typeDecl(graph, moved.id, newHost.id)

      case TypeMember =>
        controller.console.
          appendText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

        val host = graph.getConcreteNode(graph.container(moved.id).get)
        val uses = graph.usesOfUsersOf(moved.id)
        val choice =
          if(move.usedBySiblingsViaSelf(uses, graph, host)) {
            Some(MoveAction.getChoice(Field).
              getOrElse(CreateTypeMember(Field)))
          }
          else None

        move.typeMember(graph, Seq(moved.id), newHost.id, choice)(uses)
      case _ =>
        -\/(new PuckError(s"move of ${moved.kind} not implemented"))
    }) match {
      case -\/(err)  =>
        controller.console.appendText(s"Abstraction creation failure\n${err.getMessage}\n")
      case \/-(g) => controller.pushGraph(g)
    }
  }
}
