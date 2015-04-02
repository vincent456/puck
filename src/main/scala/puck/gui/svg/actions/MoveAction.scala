package puck.gui.svg.actions

import java.awt.event.ActionEvent
import javax.swing.{JOptionPane, AbstractAction}

import puck.PuckError
import puck.graph._
import puck.graph.transformations.{CreateVarStrategyForJava, CreateTypeMember, CreateVarStrategy, CreateParameter}
import puck.gui.svg.SVGController
import puck.javaGraph.nodeKind.Field

import scalaz.{Failure, Success}

/**
 * Created by lorilan on 3/18/15.
 */

object MoveAction {
  def getChoice(k : NodeKind): Option[CreateVarStrategy] = {
    val choicesArray = Array[Object](CreateTypeMember(k),
                                     CreateParameter)

    val choice = JOptionPane.showInputDialog(null, //Component parentComponent
      "Parameter or Field ?", //Object message,
      "How to get self reference",  //String title
      JOptionPane.PLAIN_MESSAGE, //int messageType
      null, //Icon icon,
      choicesArray, //Object[] options,
      choicesArray(0) //Object initialValue
    ).asInstanceOf[CreateVarStrategy]

    if(choice == null) None
    else Some(choice)
  }
}

class MoveAction
( host : DGNode,
  moved : ConcreteNode,
  controller : SVGController)
extends AbstractAction(s"Move ${moved.name} here"){

  import controller.graph

  override def actionPerformed(e: ActionEvent): Unit = {
    (graph.kindType(moved) match {
      case TypeDecl =>
        controller.transfoRules.moveTypeDecl(graph, moved.id, host.id)

      case TypeMember =>
        controller.console.
          setText("/!\\/!\\ Method overriding unchecked (TODO !!!) /!\\/!\\")

        val choice =
          if(controller.transfoRules.needSelfReference(graph, moved, host))
            MoveAction.getChoice(Field).
              getOrElse(CreateTypeMember(Field))
          else CreateTypeMember(Field)

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
