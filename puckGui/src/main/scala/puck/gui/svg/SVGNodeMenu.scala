package puck.gui.svg

import puck.graph._
import puck.gui.svg.actions.{AutoSolveAction, ManualSolveAction}
import puck.gui.{ConcreteNodeMenu, VirtualNodeMenu}
import javax.swing._

object SVGNodeMenu{

  def apply(controller : SVGController,
            nodeId : NodeId) : JPopupMenu =
    controller.graph.getNode(nodeId) match {
      case n : ConcreteNode => new SVGConcreteNodeMenu(controller, n)
      case n : VirtualNode => new VirtualNodeMenu(controller, n)
    }
}

class SVGConcreteNodeMenu
(controller: SVGController,
 node : ConcreteNode)
  extends ConcreteNodeMenu(controller, node) {

  import controller.graph

  override def init() = {
    super.init()

    if (graph.isWronglyContained(node.id)
      || graph.isWronglyUsed(node.id)) {
      this.add(new ManualSolveAction(node, controller))
      this.add(new AutoSolveAction(node, controller))
    }

    this.addSeparator()
    addShowOptions()
  }

  private def addShowOptions() : Unit = {

    this.addMenuItem("Infos"){ _ =>
      controller.showNodeInfos(node.id)
    }

    this.addMenuItem("Hide") { _ =>
      controller.hide(node.id)
    }
    this.addMenuItem("Focus") { _ =>
      controller.focusExpand(node.id, focus = true, expand = false)
    }
    this.addMenuItem("Focus & Expand") { _ =>
      controller.focusExpand(node.id, focus = true, expand = true)
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
        controller.focusExpand(node.id, focus = false, expand = true)
      }
      this.addMenuItem("Expand all") { _ =>
        controller.expandAll(node.id)
      };()
    }
  }
}



