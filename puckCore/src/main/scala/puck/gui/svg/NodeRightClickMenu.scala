package puck.gui.svg

import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.gui.svg.actions.AddIsaAction
import puck.gui.svg.actions.MergeAction
import puck.gui.svg.actions.MoveAction
import puck.gui.svg.actions.RedirectAction
import puck.gui.svg.actions.RemoveNodeAction
import puck.gui.svg.actions._
import javax.swing._
import java.awt.event.ActionEvent

object NodeRightClickMenu{

  def apply(controller : SVGController, nodeId : NodeId) : JPopupMenu =
    controller.graph.getNode(nodeId) match {
      case n : ConcreteNode => new ConcreteNodeRightClickMenu(controller, n)
      case n : VirtualNode => new VirtualNodeRightClickMenu(controller, n)
    }

  implicit class JPopupSyntax(val menu : JPopupMenu) extends AnyVal {
    def addMenuItem(name : String)(action : ActionEvent => Unit) : JMenuItem = {
      menu.add(new AbstractAction(name) {
        def actionPerformed(actionEvent: ActionEvent) : Unit =
          action(actionEvent)
      })
    }
  }
}

import NodeRightClickMenu.JPopupSyntax

class ConcreteNodeRightClickMenu
( private val controller: SVGController,
  node : ConcreteNode) extends JPopupMenu {

  init()


  import controller.graph

  def init() : Unit = {

    this.add(new RenameNodeAction(node, controller))
    this.addSeparator()

    val item = new JMenuItem(s"Abstract ${node.name} as")
    item.setEnabled(false)
    this.add(item)

    controller.abstractionChoices(node).foreach(this.add)

    val childChoices  = controller.childChoices(node)
    if (childChoices.nonEmpty) {
      this.addSeparator()
      childChoices.foreach(this.add)
    }

    this.addSeparator()
    this.add(new RemoveNodeAction(node, controller))

    controller.selectedNodes match {
      case Nil => ()
      case List((nid,_,_)) => addOtherNodeSelectedOption(nid)
      case nodes => addOtherNodesSelectedOption(nodes map (_._1))
    }



    controller.selectedEdge match {
      case Some((e, _, _)) => addEdgeSelectedOption(e)
      case _ => ()
    }


    if(graph.isWronglyContained(node.id)
        || graph.isWronglyUsed(node.id)){
      this.add(new ManualSolveAction(node, controller))
      this.add(new AutoSolveAction(node, controller))
    }

    this.addSeparator()
    addShowOptions()
  }


  private def addAddIsaOption(sub: ConcreteNode, sup: ConcreteNode) : Unit = {
    this.add(new AddIsaAction(sub, sup, controller));()
  }


  private def addOtherNodesSelectedOption(ids : List[NodeId]) : Unit = {
    val sContainer = graph.container(ids.head)
    val sameContainer = ids.tail forall(graph.container(_) == sContainer)
    val kt = graph.kindType(ids.head)
    val sameKind = ids.tail forall (graph.kindType(_) == kt)
    if(!sameContainer)
      controller.console.appendText("Move multiple only available for nodes with same container")
    else if(!sameKind)
      controller.console.appendText("Move multiple only available for nodes with same kind")
    else {
      val selected: ConcreteNode = graph.getConcreteNode(ids.head)
      if (graph.canContain(node, selected)) {
        this.add(new MoveAction(node, ids, controller))
        ()
      }
    }

  }

  private def addOtherNodeSelectedOption(id : NodeId) : Unit = {
    val selected: ConcreteNode = graph.getConcreteNode(id)
    if (graph.canContain(node, selected)) {
      this.add(new MoveAction(node, List(id), controller))
    }

//    val m: MergeMatcher = controller.transfoRules.
//        mergeMatcherInstances.syntaxicMergeMatcher(selected)
//
//    if (m.canBeMergedInto(node, graph))
      this.add(new MergeAction(selected, node, controller))


    if(selected.id != node.id) {
      if (selected.kind.canBe(node.kind)) addAddIsaOption(selected, node)
      if (node.kind.canBe(selected.kind)) addAddIsaOption(node, selected)
    }
  }

  private def addEdgeSelectedOption(edge : DGEdge) : Unit = {
    def addRedirectAction(uses : DGUses) =
      graph.abstractions(edge.target).foreach {
        abs =>
          if (abs.nodes.contains(node.id))
            this.add(new RedirectAction(node, uses, abs, controller))

      }
    edge match {
      case uses: Uses =>
        if(uses.existsIn(graph))
          addRedirectAction(uses)
        graph.definition(uses.user).foreach{
          userDef =>
            graph.getUsesEdge(userDef, uses.used).foreach{
              usesFromDef =>
                addRedirectAction(usesFromDef)
            }
        }

      case _ => ()
    }
  }


  private def addShowOptions() : Unit = {
    this.addMenuItem("Hide") { _ =>
      controller.hide(node.id)
    }

    this.addMenuItem("Show code") { _ =>
      controller.printCode(node.id)
    }

    this.addMenuItem("Show abstractions") { _ =>
      controller.printAbstractions(node.id)
    }

    if (graph.content(node.id).nonEmpty) {
      this.addMenuItem("Collapse") { _ =>
        controller.collapse(node.id)
      }
      this.addMenuItem("Expand") { _ =>
        controller.expand(node.id)
      }
      this.addMenuItem("Expand all") { _ =>
        controller.expandAll(node.id)
      };()
    }
  }
}

class VirtualNodeRightClickMenu
 ( controller: SVGController,
   node : VirtualNode
   ) extends JPopupMenu {

  node.potentialMatches foreach {
      id =>
        val consumer = controller.graph.getConcreteNode(id)
        import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}
        this.addMenuItem(s"Concretize as $consumer") { _ =>
          printErrOrPushGraph(controller,"Concretize action failure") {
            TR.mergeInto(graph.mileStone, node.id, consumer.id)
          }
        }
  }

}

