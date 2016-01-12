package puck
package gui

import javax.swing.{JMenuItem, JPopupMenu}

import puck.actions._
import puck.graph._
import puck.graph.transformations.rules.Redirection
import puck.gui.svg._

import scala.swing.Publisher

/**
  * Created by lorilan on 16/12/15.
  */

object NodeMenu{

  def apply(controller : Publisher,
            graph : DependencyGraph,
            graphUtils: GraphUtils,
            selectedNodes: List[NodeId],
            selectedEdge : Option[NodeIdP],
            nodeId : NodeId) : JPopupMenu =
    graph.getNode(nodeId) match {
      case n : ConcreteNode =>
        new ConcreteNodeMenu(controller, graph, graphUtils, selectedNodes, selectedEdge, n)
      case n : VirtualNode =>
        new VirtualNodeMenu(controller, graph, graphUtils, n)
    }
}

class ConcreteNodeMenu
(publisher: Publisher,
 implicit val graph : DependencyGraph,
 implicit val graphUtils : GraphUtils,
 val selectedNodes: List[NodeId],
 val selectedEdge : Option[NodeIdP],
 node : ConcreteNode) extends JPopupMenu {

  init()

  def init(): Unit = {

    this.add(new RenameNodeAction(publisher, node))
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
      this.add(new CreateInitalizerAction(publisher,
        graph.getConcreteNode(graph.hostTypeDecl(node.id))))
    }

    this.addSeparator()
    this.add(new SetMutabilityAction(publisher, node, !node.mutable))
    this.add(new RemoveNodeAction(publisher, node))

    selectedNodes match {
      case Nil => ()
      case List(nid) => addOtherNodeSelectedOption(nid)
      case nodes => addOtherNodesSelectedOption(nodes)
    }

    selectedEdge foreach addEdgeSelectedOption

  }

  def abstractionChoices : Seq[JMenuItem] =
    node.kind.abstractionChoices.map { case (k, p) =>
      new JMenuItem(new AbstractionAction(publisher, node, p, k))
    }

  def childChoices : Seq[JMenuItem] = {
    val ks = graph.nodeKinds.filter(node.kind.canContain)
    ks map {k => new JMenuItem(new AddNodeAction(publisher, node, k))}
  }

  private def addAddIsaOption(sub: ConcreteNode, sup: ConcreteNode): Unit = {
    ignore( this add new AddIsaAction(publisher, sub, sup) )

  }


  private def addOtherNodesSelectedOption(ids: List[NodeId]): Unit = {
    val sContainer = graph.container(ids.head)
    val sameContainer = ids.tail forall (graph.container(_) == sContainer)
    val kt = graph.kindType(ids.head)
    val sameKind = ids.tail forall (graph.kindType(_) == kt)
    if (!sameContainer)
      publisher.publish(Log("Move multiple only available for nodes with same container"))
    else if (!sameKind)
      publisher.publish(Log("Move multiple only available for nodes with same kind"))
    else {
      val selected: ConcreteNode = graph.getConcreteNode(ids.head)
      if (graph.canContain(node, selected))
        ignore( this add new MoveAction(publisher, node, ids) )


    }

  }

  private def addOtherNodeSelectedOption(id: NodeId): Unit = {
    val selected: ConcreteNode = graph.getConcreteNode(id)
    if (graph.canContain(node, selected)) {
      this.add(new MoveAction(publisher, node, List(id)))
    }

    //    val m: MergeMatcher = controller.transfoRules.
    //        mergeMatcherInstances.syntaxicMergeMatcher(selected)
    //
    //    if (m.canBeMergedInto(node, graph))
    if (selected.kind.kindType == node.kind.kindType)
      this.add(new MergeAction(publisher, selected, node))


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
            this.add(new RedirectAction(publisher, uses, abs))
      }

    def addChangeInitUsesAction(ctorDef: NodeId) =
      (graph.getRole(node.id), graph.getRole(target)) match {
        case (Some(Factory(ctorId)), Some(Initializer(_)))
          if ctorId == source =>
          this.add(abstractAction("Call to initialization in factory") {
            _ =>
              val g = Redirection.redirectSourceOfInitUseInFactory(graph.mileStone,
                ctorId, ctorDef, target, node.id)

              publisher.publish(PushGraph(g))
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
(controller: Publisher,
 graph : DependencyGraph,
 graphUtils : GraphUtils,
 node : VirtualNode
) extends JPopupMenu {

  node.potentialMatches foreach {
    id =>
      val consumer = graph.getConcreteNode(id)
      import graphUtils.{transformationRules => TR}
      this.addMenuItem(s"Concretize as $consumer") { _ =>
        printErrOrPushGraph(controller,"Concretize action failure") {
          TR.merge.mergeInto(graph.mileStone, node.id, consumer.id)
        }
      }
  }

}