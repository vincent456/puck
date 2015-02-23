package puck.transformations

import puck.graph.DependencyGraph.NodeId
import puck.graph.{NoType, DependencyGraph}
import puck.AcceptanceSpec
import puck.graph.constraints.SupertypeAbstraction
import puck.javaAG.nodeKind._
import puck.javaAG.{JavaTransformationRules => TR, CompileHelper}

import scalaz._
import scalaz.Validation.FlatMap._
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

  type TryG = puck.graph.Try[DependencyGraph]

  val compare: (DependencyGraph, DependencyGraph) => Boolean =
    (g1, g2) => DependencyGraph.areEquivalent(initialRecord,g1,g2)

  def liftAssert(tg1 : TryG, tg2 : TryG, expected : Boolean): Unit = {

    val app = Applicative[puck.graph.Try]
    val res = app.apply2(tg1, tg2)(compare)
    res match {
      case Success(b) => assert(b === expected)
      case Failure(_) => assert(false)
    }

  }

  feature("Comparison"){
    scenario("Same transformations, intro Interface"){

      def seq() = TR.createAbstraction(graph, classB, Interface, SupertypeAbstraction)
        .map {case (classBAbsId, g) => g.addContains(packageMethodUsesViaThisField, classBAbsId)}

      val t1 = seq()

      val t2 = seq()

      liftAssert(t1, t2, expected = true)
    }

    scenario("Same transformations, intro Interface, intro package, move interface in new package "){
      def seq(pname : String) =
        TR.createAbstraction(graph, classB, Interface, SupertypeAbstraction)

        .map {case (classBAbsId, g) =>
          (g.addContains(packageMethodUsesViaThisField, classBAbsId), classBAbsId)}

        .map {case (g : DependencyGraph, itcId : NodeId) =>
            val (pid, g2) = g.addNode(pname, Package, NoType)
          (g2.addContains(packageMethodUsesViaThisField, pid), pid, itcId)}

        .flatMap {case (g : DependencyGraph, pid, itcId) =>
          TR.moveTo(g, itcId, pid)}

      val t1 = seq("p1")

      val t2 = seq("p2")

      liftAssert(t1, t2, expected = true)
    }

    scenario("Different transformations, intro Interface, intro (different) package, move interface in new package "){
      def seq(pname : String, pcontainer : NodeId) =
        TR.createAbstraction(graph, classB, Interface, SupertypeAbstraction)

          .map {case (classBAbsId, g) =>
          (g.addContains(packageMethodUsesViaThisField, classBAbsId), classBAbsId)}

          .map {case (g : DependencyGraph, itcId : NodeId) =>
          val (pid, g2) = g.addNode(pname, Package, NoType)
          (g2.addContains(pcontainer, pid), pid, itcId)}

          .flatMap {case (g : DependencyGraph, pid, itcId) =>
          TR.moveTo(g, itcId, pid)}

      val t1 = seq("p1", packageMethodUsesViaThisField)

      val t2 = seq("p2", graph.rootId)

      liftAssert(t1, t2, expected = false)
    }

  }

}
