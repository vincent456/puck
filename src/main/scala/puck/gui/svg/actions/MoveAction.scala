package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph._
import puck.graph.transformations.rules._
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.Field


import scala.swing.Dialog
import scala.swing.Swing.EmptyIcon
import scalaz.{\/-, -\/}

/**
 * Created by lorilan on 3/18/15.
 */

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

  import controller.graph

  override def actionPerformed(e: ActionEvent): Unit = {
    (graph.kindType(moved) match {
      case TypeDecl =>
        Move.moveTypeDecl(graph, moved.id, newHost.id)

      case TypeMember =>
        controller.console.
          setText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

        val host = graph.getConcreteNode(graph.container(moved.id).get)
        val choice =
          if(Move.isUsedBySiblingsViaSelf(graph, moved, host)) {
            MoveAction.getChoice(Field).
              getOrElse(CreateTypeMember(Field))
          }
          else CreateTypeMember(Field)

        Move.moveTypeMember(graph, moved.id, newHost.id, choice)
      case _ =>
        -\/(new PuckError(s"move of ${moved.kind} not implemented"))
    }) match {
      case -\/(err)  =>
        controller.console.appendText(s"Abstraction creation failure\n${err.getMessage}\n")
      case \/-(g) => controller.pushGraph(g)
    }
  }
}
