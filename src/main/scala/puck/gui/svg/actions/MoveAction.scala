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

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}

  override def actionPerformed(e: ActionEvent): Unit = {
    val g = graph.mileStone
    (g.kindType(moved.head) match {
      case TypeDecl =>
        Collections.traverse(moved, g){
          (g, id) => TR.move.typeDecl(g, id, newHost.id)
        }


      case TypeMember =>
        controller.console.
          appendText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

        val host = g.getConcreteNode(g.container(moved.head).get)
        val uses = g.usesOfUsersOf(moved)
        val choice =
          if(TR.move.usedBySiblingsViaSelf(uses, g, host)) {
            Some(MoveAction.getChoice(Field).
              getOrElse(CreateTypeMember(Field)))
          }
          else None

        TR.move.typeMember(g, moved, newHost.id, choice)(uses)
      case kt =>
        -\/(new PuckError(s"move of $kt not implemented"))
    }) match {
      case -\/(err)  =>
        controller.console.appendText(s"Abstraction creation failure\n${err.getMessage}\n")
      case \/-(g) => controller.pushGraph(g)
    }
  }
}
