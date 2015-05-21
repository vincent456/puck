package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph._
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.Field
import puck.util.Collections


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


  val move = controller.transfoRules.move

  override def actionPerformed(e: ActionEvent): Unit = {
    val graph = controller.graph.mileStone
    (graph.kindType(moved.head) match {
      case TypeDecl =>
        Collections.traverse(moved, graph){
          (g, id) => move.typeDecl(g, id, newHost.id)
        }


      case TypeMember =>
        controller.console.
          appendText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

        val host = graph.getConcreteNode(graph.container(moved.head).get)
        val uses = graph.usesOfUsersOf(moved)
        val choice =
          if(move.usedBySiblingsViaSelf(uses, graph, host)) {
            Some(MoveAction.getChoice(Field).
              getOrElse(CreateTypeMember(Field)))
          }
          else None

        move.typeMember(graph, moved, newHost.id, choice)(uses)
      case kt =>
        -\/(new PuckError(s"move of $kt not implemented"))
    }) match {
      case -\/(err)  =>
        controller.console.appendText(s"Abstraction creation failure\n${err.getMessage}\n")
      case \/-(g) => controller.pushGraph(g)
    }
  }
}
