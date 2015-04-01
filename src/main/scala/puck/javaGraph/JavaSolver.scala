package puck.javaGraph

import puck.graph.constraints.{DecisionMaker, Solver}
import puck.javaGraph.nodeKind._
import puck.javaGraph.transformations.JavaTransformationRules


/**
 * Created by lorilan on 28/05/14.
 */

object JavaSolver{
  def apply(decisionMaker : DecisionMaker,
            automaticConstraintLoosening : Boolean) =
    new Solver(decisionMaker,
               JavaTransformationRules,
               automaticConstraintLoosening)

  val violationPrioritySeq =
              Seq[JavaNodeKind]( Field, Constructor, Class, Interface)
}
