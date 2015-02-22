package puck.transformations

import puck.AcceptanceSpec
import puck.graph.constraints.SupertypeAbstraction
import puck.javaAG.nodeKind.Interface
import puck.javaAG.{JavaTransformationRules, CompileHelper}

import scalaz.{ValidationFunctions, Applicative}


/**
 * Created by lorilan on 22/02/15.
 */


class RecordingComparatorSpec extends AcceptanceSpec {

  info("A recording comparator should be able to tell if two refactoring plan are equal or not")

  val (p, graph, initialRecord, fullname2id, dg2ast) =
    CompileHelper.buildGraph(List(puck.testExamplesPath + "/methodUsesViaThisField/A.java"), List())

  val packageMethodUsesViaThisField = fullname2id("examples.methodUsesViaThisField")

  val classA = fullname2id("examples.methodUsesViaThisField.A")
  val classB = fullname2id("examples.methodUsesViaThisField.B")


  feature("Comparison"){
    scenario("Same transformation"){
      val t1 = JavaTransformationRules.createAbstraction(graph, classB, Interface, SupertypeAbstraction)
      .map {case (classBAbsId, g) => g.addContains(packageMethodUsesViaThisField, classBAbsId)}

      val t2 = JavaTransformationRules.createAbstraction(graph, classB, Interface, SupertypeAbstraction)
        .map {case (classBAbsId, g) => g.addContains(packageMethodUsesViaThisField, classBAbsId)}

      assert(true)
    }
  }

}
