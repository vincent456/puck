package puck.view.search

import scala.swing._

import puck.graph.constraints.ConstraintsMaps
import puck.graph.{DependencyGraph, Metrics}
import puck.graph.constraints.ConstraintsMapsUtils._


/**
  * Created by Loïc Girault on 12/2/16.
  */
abstract class MetricChoice {
  def metric : DependencyGraph => Int
  def hasParameters : Boolean = parametersPanel.nonEmpty
  def parametersPanel : Option[Component]
}

object MetricChoice {
  def dialog(metricChoices : MetricChoice*) : Option[DependencyGraph => Int] = {
    import Dialog._

    Dialog.showInput(
      message = "Choose your metric",
      title = "Metric choice",
      entries = metricChoices,
      initial = metricChoices.head) match {
      case None => None
      case Some(mc) if ! mc.hasParameters => Some(mc.metric)
      case Some(mc) =>
        mc.parametersPanel.flatMap {
          paramPanel =>
            Dialog.showConfirmation(
              message = paramPanel.peer,
              title = "Metric parameters",
              optionType = Options.OkCancel) match {
              case Result.Ok =>
                Some(mc.metric)
              case Result.Cancel =>
                dialog(metricChoices:_*)
            }
        }



    }
  }

  def metric1(cm : ConstraintsMaps) = new MetricChoice {
    mc =>

    override val toString = "Metric 1"

    val kViolTextField = new TextField("1", 5)
    val kComplexTextField = new TextField("1", 5)

    override def hasParameters : Boolean = true
    def parametersPanel : Option[Component] =
      Some(new BoxPanel(Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label("Violation weight")
          contents += kViolTextField
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label("Complexity weight")
          contents += kComplexTextField
        }
      })

    def metric : (DependencyGraph) => Int = {
      val kViol = kViolTextField.text.toInt
      val kComplex = kComplexTextField.text.toInt

      new ((DependencyGraph) => Int) {
        override val toString : String = mc.toString
        def apply( g : DependencyGraph) =
          Metrics.fitness1(g, cm, kViol, kComplex)
      }

    }
  }


  def metric2(dependencyGraph: DependencyGraph, constraintsMaps: ConstraintsMaps) =
    new MetricChoice {
      mc =>

      override val toString = "Metric 2"

      val kViolTextField = new TextField("1", 5)
      val kComplexTextField = new TextField("1", 5)

      def parametersPanel : Option[Component] = None

      def metric : (DependencyGraph) => Int = {
        val nodesSet = dependencyGraph nodesIn constraintsMaps

        new ((DependencyGraph) => Int) {
          override val toString : String = mc.toString
          def apply( g : DependencyGraph) =
            Metrics.fitness2(g, nodesSet)
        }
      }
    }

}

