package puck.gui.decisionsFrames

import scala.swing._
import puck.graph.{NodeKind, AGNode}
import scala.swing.event.{SelectionChanged, Event}
import puck.graph.constraints.{AbstractionPolicy, RedirectionPolicy}
import scala.concurrent.Promise
import puck.javaAG.DefaultDecisionMaker
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.Success

/**
 * Created by lorilan on 04/06/14.
 */
class AbstractionKindAndPolicyChooser(val impl : AGNode)
  extends DecisionFrame[(NodeKind, AbstractionPolicy)] {

  title = "Choose abstraction kind and policy"

  val policyChoice = new ComboBox(impl.kind.abstractionPolicies)
  var absPolicy : AbstractionPolicy = policyChoice.selection.item
  var kindChoice = new ComboBox(impl.kind.abstractKinds(absPolicy))
  var absKind : NodeKind = kindChoice.selection.item

  val kindChoiceWrapper = new FlowPanel(){ contents += kindChoice}
  listenTo(policyChoice.selection)

  reactions += {
    case SelectionChanged(source) =>
      if(source == policyChoice) {
        absPolicy = policyChoice.selection.item

        kindChoice = new ComboBox(impl.kind.abstractKinds(absPolicy))

        kindChoiceWrapper.contents.clear()
        kindChoiceWrapper.contents += kindChoice
        AbstractionKindAndPolicyChooser.this.pack()
      }
    //case e : Event => println(e.getClass +" happened !")
  }

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new Label("Abstracting " + impl)

    contents +=  new FlowPanel(){
      contents += policyChoice
      contents += kindChoiceWrapper
    }

    val default = DefaultDecisionMaker.abstractionKindAndPolicy(impl)

    contents += new Label("Default decision is " + default)
    contents += new FlowPanel(){
      contents += Button("Default"){
        AbstractionKindAndPolicyChooser.this.complete(default)
      }
      contents += Button("OK") {
        AbstractionKindAndPolicyChooser.this.complete((absKind, absPolicy))
      }

    }
  }
}
