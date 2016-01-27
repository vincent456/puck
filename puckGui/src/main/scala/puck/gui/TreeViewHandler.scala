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
    this deafTo mainPanel.control
    mainPanel.control deafTo graphExplorer
    mainPanel.viewHandler = new SVGViewHandler(mainPanel)
  }

  import mainPanel.control

  val graphExplorer =
    new GraphExplorer(mainPanel.control,
      treeIcons,
      control.graphUtils,
      control.printingOptionsControl)

  control listenTo graphExplorer
  this listenTo control

  reactions += {
    case GraphUpdate(graph) =>
      update(graph, None)
    case GraphFocus(graph, edge) =>
      update(graph, Some(Left(edge)))
    case VisibilityEvent(v) =>
      println("tvh receives vis evt")
      import puck.graph.io.VisibilitySet.VisibilitySetOps
      update(control.graph, Some(Right(v.visibleNodes(control.graph))))

  }


  mainPanel.upPanel.setGraphView(graphExplorer)

  def update(graph : DependencyGraph, se : Option[Either[DGEdge, Set[NodeId]]]) : Unit = {
    graphExplorer.display(graph, se)
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
              new ConstraintViolationExplorer(mainPanel.interface,
                violations, treeIcons,
                control.printingOptionsControl)(graph,
                control.graphUtils)
            contents += constraintViolationExplorer
           }, 0.5)
    mainPanel.downPanel.leftComponent = c
    mainPanel.downPanel.resizeWeight = rw
  }


}

