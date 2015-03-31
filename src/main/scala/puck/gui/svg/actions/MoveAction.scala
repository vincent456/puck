package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph._
import puck.graph.transformations.CreateParameter
import puck.gui.svg.{ParamOrFieldChoice, SVGController}
import puck.javaGraph.nodeKind.Field

import scalaz.{Failure, Success}

/**
 * Created by lorilan on 3/18/15.
 */
class MoveAction
( host : DGNode,
  moved : DGNode,
  graph : DependencyGraph,
  controller : SVGController)
extends AbstractAction(s"Move ${moved.name(graph)} here"){

  def needSelfReference : Boolean = {
    def sibling: NodeId => Boolean =
      sid => graph.contains(host.id, sid) && sid != moved.id

    def selfTypeUse(usedId : NodeId) = {
      val tuses = graph.typeUsesOf((moved.id, usedId))
      tuses.isEmpty || tuses.exists(DGEdge.uses(_).selfUse)
    }

    graph.usedBy(moved.id).filter{
      used => sibling(used) && selfTypeUse(used)
    }.nonEmpty
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    (graph.kindType(moved) match {
      case TypeDecl =>
        controller.transfoRules.moveTypeDecl(graph, moved.id, host.id)

      case TypeMember =>
        controller.console.
          setText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

        val choice =
          if(needSelfReference) ParamOrFieldChoice.getChoice(Field)
          else CreateParameter

        controller.transfoRules.moveTypeMember(graph, moved.id, host.id, choice)
      case _ =>
        Failure(new PuckError(s"move of ${moved.kind} not implemented")).toValidationNel
    }) match {
      case Failure(errs) =>
        controller.console.appendText("Abstraction creation failure\n" )
        errs.foreach(e => controller.console.appendText(e.getMessage + "\n"))
      case Success(g) => controller.pushGraph(g)
    }
  }
}
