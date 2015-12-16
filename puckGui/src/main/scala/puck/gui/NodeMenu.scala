package puck
package gui

import javax.swing.{JMenuItem, JPopupMenu}

import puck.actions._
import puck.graph._
import puck.graph.transformations.rules.Redirection
import puck.gui.svg._

/**
  * Created by lorilan on 16/12/15.
  */

object NodeMenu{

  def apply(controller : GraphController, nodeId : NodeId) : JPopupMenu =
    controller.graph.getNode(nodeId) match {
      case n : ConcreteNode => new ConcreteNodeMenu(controller, n)
      case n : VirtualNode => new VirtualNodeMenu(controller, n)
    }
}

class ConcreteNodeMenu
( controller: GraphController,
  node : ConcreteNode) extends JPopupMenu {

  init()

  import controller.graph

  def init(): Unit = {

    this.add(new RenameNodeAction(controller, node))
    this.addSeparator()

    val item = new JMenuItem(s"Abstract ${node.name} as")
    item.setEnabled(false)
    this.add(item)

    abstractionChoices foreach this.add

    if (childChoices.nonEmpty) {
      this.addSeparator()
      childChoices.foreach(this.add)
    }

    if (node.kind.isWritable) {
      this.add(new CreateInitalizerAction(controller,
        graph.getConcreteNode(graph.hostTypeDecl(node.id))))
    }

    this.addSeparator()
    this.add(new RemoveNodeAction(controller, node))

    controller.selectedNodes match {
      case Nil => ()
      case List(nid) => addOtherNodeSelectedOption(nid)
      case nodes => addOtherNodesSelectedOption(nodes)
    }

    controller.selectedEdge foreach addEdgeSelectedOption

  }

  def abstractionChoices : Seq[JMenuItem] =
    node.kind.abstractionChoices.map { case (k, p) =>
      new JMenuItem(new AbstractionAction(controller, node, p, k))
    }

  def childChoices : Seq[JMenuItem] = {
    val ks = graph.nodeKinds.filter(node.kind.canContain)
    ks map {k => new JMenuItem(new AddNodeAction(controller, node, k))}
  }

  private def addAddIsaOption(sub: ConcreteNode, sup: ConcreteNode): Unit = {
    ignore( this add new AddIsaAction(controller, sub, sup) )

  }


  private def addOtherNodesSelectedOption(ids: List[NodeId]): Unit = {
    val sContainer = graph.container(ids.head)
    val sameContainer = ids.tail forall (graph.container(_) == sContainer)
    val kt = graph.kindType(ids.head)
    val sameKind = ids.tail forall (graph.kindType(_) == kt)
    if (!sameContainer)
      controller.logger.writeln("Move multiple only available for nodes with same container")
    else if (!sameKind)
      controller.logger.writeln("Move multiple only available for nodes with same kind")
    else {
      val selected: ConcreteNode = graph.getConcreteNode(ids.head)
      if (graph.canContain(node, selected))
        ignore( this add new MoveAction(controller, node, ids) )


    }

  }

  private def addOtherNodeSelectedOption(id: NodeId): Unit = {
    val selected: ConcreteNode = graph.getConcreteNode(id)
    if (graph.canContain(node, selected)) {
      this.add(new MoveAction(controller, node, List(id)))
    }

    //    val m: MergeMatcher = controller.transfoRules.
    //        mergeMatcherInstances.syntaxicMergeMatcher(selected)
    //
    //    if (m.canBeMergedInto(node, graph))
    if (selected.kind.kindType == node.kind.kindType)
      this.add(new MergeAction(controller, selected, node))


    if (selected.id != node.id) {
      if (selected.kind.canBe(node.kind)) addAddIsaOption(selected, node)
      if (node.kind.canBe(selected.kind)) addAddIsaOption(node, selected)
    }
  }

  private def addEdgeSelectedOption(edge: NodeIdP): Unit = {
    val (source, target) = edge
    def addRedirectAction(uses: Uses) =
      graph.abstractions(target).foreach {
        abs =>
          if (abs.nodes.contains(node.id))
            this.add(new RedirectAction(controller, node, uses, abs))
      }

    def addChangeInitUsesAction(ctorDef: NodeId) =
      (graph.getRole(node.id), graph.getRole(target)) match {
        case (Some(Factory(ctorId)), Some(Initializer(_)))
          if ctorId == source =>
          this.add(abstractAction("Call to initialization in factory") {
            _ =>
              val g = Redirection.redirectSourceOfInitUseInFactory(controller.graph.mileStone,
                ctorId, ctorDef, target, node.id)

              controller pushGraph g
          })
        case _ => ()
      }

    graph.getUsesEdge(source, target).foreach {
      uses => addRedirectAction(uses)
    }

    graph.definitionOf(source).foreach {
      userDef =>
        graph.getUsesEdge(userDef, target).foreach {
          usesFromDef =>
            addRedirectAction(usesFromDef)
            addChangeInitUsesAction(userDef)
        }
    }


  }
}

class VirtualNodeMenu
(controller: GraphController,
 node : VirtualNode
) extends JPopupMenu {

  node.potentialMatches foreach {
    id =>
      val consumer = controller.graph.getConcreteNode(id)
      import controller.{graph, graphUtils}, graphUtils.{transformationRules => TR}
      this.addMenuItem(s"Concretize as $consumer") { _ =>
        printErrOrPushGraph(controller,"Concretize action failure") {
          TR.merge.mergeInto(graph.mileStone, node.id, consumer.id)
        }
      }
  }

}