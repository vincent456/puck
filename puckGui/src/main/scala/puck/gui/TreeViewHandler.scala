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
   implicit val treeIcons : DGTreeIcons)
  extends ViewHandler with Publisher {

  def switchView(mainPanel: PuckMainPanel, treeIcons: DGTreeIcons) : Unit = {
    this deafTo mainPanel.control.Bus
    mainPanel.viewHandler = new SVGViewHandler(mainPanel)
  }

  import mainPanel.control

  val graphExplorer =
    new GraphExplorer(control.Bus,
      control.graphUtils,
      control.printingOptionsControl)

  this listenTo control.Bus

  reactions += {
    case _: GraphStackEvent => update(control.graph)
  }


  mainPanel.upPanel.setGraphView(graphExplorer)



  import mainPanel.downPanel
  def update(graph : DependencyGraph) : Unit = {
    val violations = graph.violations()
      if (violations.isEmpty) {
        downPanel.orientation = Orientation.Horizontal
        downPanel.leftComponent = new Label("0 violation !")
        downPanel.resizeWeight = 0
        downPanel.dividerSize = 0
      }
      else {
        downPanel.orientation = Orientation.Vertical
        downPanel.leftComponent = new BoxPanel(Orientation.Vertical) {
          contents += new Label("Constraints Violations")
          val constraintViolationExplorer =
            new ConstraintViolationExplorer(control.Bus, violations,
              control.printingOptionsControl)(graph,
              control.graphUtils,
              treeIcons)
          contents += constraintViolationExplorer
        }
        downPanel.resizeWeight = 0.5
        downPanel.dividerSize = 3
      }
    downPanel.repaint()

  }


}

