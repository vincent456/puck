package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

import puck.PuckError
import puck.graph._
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, CreateVarStrategy}
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
( newHost : DGNode,
  moved : List[NodeId],
  controller : SVGController)
extends AbstractAction(MoveAction.label(controller.graph, moved, newHost)){

  import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}
  import graphUtils.nodeKindKnowledge.kindOfKindType

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
        controller.console.
          appendText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

        val oldContainer = g.container_!(moved.head)

        val isPullUp = g.isa_*(oldContainer, newHost.id)
        val isPushDown = g.isa_*(newHost.id, oldContainer)

        controller.console.
          appendText("isPullUp = " + isPullUp)
        controller.console.
          appendText("isPushDown = " + isPushDown)

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

  override def actionPerformed(e: ActionEvent): Unit =
    printErrOrPushGraph(controller, "Abstraction creation failure" )( doMove )


}
