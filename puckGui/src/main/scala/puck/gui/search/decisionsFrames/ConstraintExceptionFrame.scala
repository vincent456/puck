/*
package puck.gui.search.decisionsFrames

import puck.graph.DGNode
import puck.graph.constraints.{Constraint, RangeSet}

import scala.swing._



/**
 * Created by lorilan on 06/06/14.
 */

object ConstraintExceptionFrame{
  def apply(sources : RangeSet, target : DGNode) : Unit =  {
    DecisionFrame {
      () => new ConstraintExceptionFrame(sources, target)
    }
  }
}

class ConstraintExceptionFrame private (val sources : RangeSet,
                                        val target : DGNode)
  extends DecisionFrame[Unit]{

  title = "Constraint Exceptions"

  def violatedScopeConstraints() : List[Constraint] = ???
    //sources.map(_.violatedScopeConstraintsOf(target)).flatten.toList
  def violatedElementConstraints() : List[Constraint] = ???
    //sources.map(_.violatedElementConstraintOf(target)).flatten.toList

  def makePanel() : Panel =  new BoxPanel(Orientation.Vertical) {

    val vsc = violatedScopeConstraints()
    val vec = violatedElementConstraints()

    val callMakePanel = { () => ConstraintExceptionFrame.this.contents = makePanel() }

    contents += new TextArea("Arc(s) targeting " + target + "comming from \n" +
      sources.mkString("\n") + "\nviolates the following constraints :" ){
      editable = false
    }

    contents += new BoxPanel(Orientation.Vertical){


      def constraintEditor[T <: Constraint, U<:DecisionFrame[Unit]](ct : T,
                                                                    getPanel : (T, RangeSet, DGNode, () => Unit) => Panel ) : Unit = {
        contents += new BoxPanel(Orientation.Horizontal){
          contents += new Label(ct.toString)
          contents += Swing.HGlue
          contents += Button("Edit"){
            ConstraintExceptionFrame.this.contents = getPanel(ct, sources, target, callMakePanel)
          }
        }
        ()
      }

      vsc.foreach{ constraintEditor(_, ScopeConstraintEditor.apply) }
      vec.foreach{ constraintEditor(_, ElementConstraintEditor.apply)}

    }

    contents += new BoxPanel(Orientation.Horizontal){
      contents += Button("Raw Editor"){
        ConstraintExceptionFrame.this.contents = ???
        /*new RawConstraintEditor(target.graph,
          target.graph.nodeSets.values.toList,
          vsc ::: vec, callMakePanel)*/
      }

      /*contents += Button("Raw Editor (all constraints)"){
        ConstraintExceptionFrame.this.contents = ???
          /*new RawConstraintEditor(target.graph,
          target.graph.nodeSets.values.toList,
          target.graph.constraints.toList, callMakePanel)*/
      }
      contents += Button("OK"){
        ConstraintExceptionFrame.this.complete(())
      }*/
    }


  }

  contents = makePanel()
}
*/
