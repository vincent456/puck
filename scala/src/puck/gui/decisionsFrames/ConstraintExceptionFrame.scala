package puck.gui.decisionsFrames

import puck.graph.{NodeKind, AGNode}
import puck.graph.constraints._
import scala.swing._



/**
 * Created by lorilan on 06/06/14.
 */

object ConstraintExceptionFrame{
  def apply[Kind <: NodeKind[Kind]] (sources : NodeSet[Kind], target : AGNode[Kind]) {
    DecisionFrame {
      () => new ConstraintExceptionFrame(sources, target)
    }
  }
}

class ConstraintExceptionFrame[Kind <: NodeKind[Kind]] private (val sources : NodeSet[Kind],
                                        val target : AGNode[Kind])
  extends DecisionFrame[Unit]{

  title = "Constraint Exceptions"

  def violatedScopeConstraints() : List[ScopeConstraint[Kind]] =
    sources.map(_.violatedScopeConstraintsOf(target)).flatten.toList
  def violatedElementConstraints() : List[ElementConstraint[Kind]] =
    sources.map(_.violatedElementConstraintOf(target)).flatten.toList

  def makePanel() : Panel =  new BoxPanel(Orientation.Vertical) {

    val vsc = violatedScopeConstraints()
    val vec = violatedElementConstraints()

    val callMakePanel = { () => ConstraintExceptionFrame.this.contents = makePanel() }

    contents += new TextArea("Arc(s) targeting " + target + "comming from \n" +
      sources.mkString("\n") + "\nviolates the following constraints :" ){
      editable = false
    }

    contents += new BoxPanel(Orientation.Vertical){


      def constraintEditor[T <: Constraint[Kind], U<:DecisionFrame[Unit]](ct : T,
                                                                    getPanel : (T, NodeSet[Kind], AGNode[Kind], () => Unit) => Panel ){
        contents += new BoxPanel(Orientation.Horizontal){
          contents += new Label(ct.toString)
          contents += Swing.HGlue
          contents += Button("Edit"){
            ConstraintExceptionFrame.this.contents = getPanel(ct, sources, target, callMakePanel)
          }
        }
      }

      vsc.foreach{ constraintEditor(_, ScopeConstraintEditor.apply) }
      vec.foreach{ constraintEditor(_, ElementConstraintEditor.apply)}

    }

    contents += new BoxPanel(Orientation.Horizontal){
      contents += Button("Raw Editor"){
        ConstraintExceptionFrame.this.contents = new RawConstraintEditor(target.graph,
          target.graph.nodeSets.values.toList,
          vsc ::: vec, callMakePanel)
      }
      contents += Button("Raw Editor (all constraints)"){
        ConstraintExceptionFrame.this.contents = new RawConstraintEditor(target.graph,
          target.graph.nodeSets.values.toList,
          target.graph.constraints.toList, callMakePanel)
      }
      contents += Button("OK"){
        ConstraintExceptionFrame.this.complete(())
      }
    }


  }

  contents = makePanel()
}
