package puck.gui


import puck.graph._
import puck.gui.explorer.{GraphExplorer, DGTreeIcons, ConstraintViolationExplorer}
import puck.gui.svg.SVGViewHandler

import scala.swing._

/**
  * Created by lorilan on 26/01/16.
  */
class TreeViewHandler
  (mainPanel : PuckMainPanel,
   treeIcons : DGTreeIcons)
  extends ViewHandler with Publisher {

  def switchView(mainPanel: PuckMainPanel, treeIcons: DGTreeIcons) : Unit = {
    this deafTo mainPanel.control.Bus
    mainPanel.viewHandler = new SVGViewHandler(mainPanel)
  }

  import mainPanel.control

  val graphExplorer =
    new GraphExplorer(control.Bus,
      treeIcons,
      control.graphUtils,
      control.printingOptionsControl)

  this listenTo control.Bus

  reactions += {
    case GraphUpdate(graph) =>
      update(graph, None)
  }


  mainPanel.upPanel.setGraphView(graphExplorer)

  def update(graph : DependencyGraph, se : Option[Either[DGEdge, Set[NodeId]]]) : Unit = {
    updateLeftOfPanel(graph)
    mainPanel.repaint()
  }

  def updateLeftOfPanel(graph : DependencyGraph) : Unit = {
    val violations = graph.violations()
    val (c, rw) : (Component, Double) =
      if (violations.isEmpty) (new Label("0 violations !"), 0)
      else (new BoxPanel(Orientation.Vertical) {
            contents += new Label("Constraints Violations")
            val constraintViolationExplorer =
              new ConstraintViolationExplorer(control.Bus,
                violations, treeIcons,
                control.printingOptionsControl)(graph,
                control.graphUtils)
            contents += constraintViolationExplorer
           }, 0.5)
    mainPanel.downPanel.leftComponent = c
    mainPanel.downPanel.resizeWeight = rw
  }


}

