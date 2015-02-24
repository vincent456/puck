package puck.graph
package transformations

import puck.util.{PuckLogger, PuckFileLogger, PuckNoopLogger}
import puck.{PuckError, AcceptanceSpec}
import puck.graph.constraints.SupertypeAbstraction
import puck.javaAG.nodeKind._
import puck.javaAG.{JavaTransformationRules => TR, ASTNodeLink, CompileHelper}

import scala.collection.mutable
import scalaz._
import scalaz.Validation.FlatMap._
/**
 * Created by lorilan on 22/02/15.
 */

import scala.language.reflectiveCalls

case class ExampleSample
( program : AST.Program,
  graph : DependencyGraph,
  initialRecord : Recording,
  fullName2id : mutable.Map[String, NodeId],
  dg2ast : Map[NodeId, ASTNodeLink]){

  def this(t : (AST.Program,
  DependencyGraph,
  Recording,
  mutable.Map[String, NodeId],
  Map[NodeId, ASTNodeLink])) =
    this(t._1, t._2, t._3, t._4, t._5)

  def this(filePath : String) = this(CompileHelper.buildGraph(List(filePath), List()))

  var logger : PuckLogger = PuckNoopLogger
  def compare: (DependencyGraph, DependencyGraph) => Boolean =
    (g1, g2) => DependencyGraph.areEquivalent(initialRecord,g1,g2, logger)
}

class RecordingComparatorSpec extends AcceptanceSpec {

  info("A recording comparator should be able to tell if two refactoring plan are equal or not")

  feature("Comparison"){

    type TryG = puck.graph.Try[DependencyGraph]

    type GraphT = DependencyGraph

    def introBInterface(classId : NodeId, pcontainer : NodeId)(g : GraphT) : Try[(GraphT, NodeId)]= {
      TR.createAbstraction(g, classId, Interface, SupertypeAbstraction)
          .map {case (classBAbsId, g) =>
          (g.addContains(pcontainer, classBAbsId), classBAbsId)}

    }

    def introPackage(pname : String, pcontainer : NodeId)(g : GraphT) : (GraphT, NodeId) = {
      val (pid, g2) = g.addNode(pname, Package, NoType)
      (g2.addContains(pcontainer, pid), pid)
    }

    def introItcPackageAndMove(classId : NodeId, pname : String, pcontainer : NodeId)(graph : GraphT) =
      graph.container(classId) match {
        case None => throw new PuckError()
        case Some(classContainer) =>
          introBInterface(classId, classContainer)(graph)

            .map { case (g, itcId) => (introPackage(pname, pcontainer)(g), itcId)}

            .flatMap { case ((g, pid), itcId) => TR.moveTo(g, itcId, pid) map ((_, pid, itcId))}
      }

    val ex1 = new ExampleSample(puck.testExamplesPath + "/methodUsesViaThisField/A.java"){
      val packageMethodUsesViaThisField = fullName2id("examples.methodUsesViaThisField")

      val classA = fullName2id("examples.methodUsesViaThisField.A")
      val classB = fullName2id("examples.methodUsesViaThisField.B")

      def seq(pname : String, pcontainer : NodeId) =
        introItcPackageAndMove(classB, pname, pcontainer)(graph) map (_._1)

    }

    def liftAssert(ex : ExampleSample, tg1 : TryG, tg2 : TryG, expected : Boolean): Unit = {

      val app = Applicative[puck.graph.Try]
      val res = app.apply2(tg1, tg2)(ex.compare)
      res match {
        case Success(b) => assert(b === expected)
        case Failure(_) => assert(false)
      }
    }



    scenario("Same transformations, intro Interface"){

      def seq() = TR.createAbstraction(ex1.graph, ex1.classB, Interface, SupertypeAbstraction)
        .map {case (classBAbsId, g) => g.addContains(ex1.packageMethodUsesViaThisField, classBAbsId)}

      val t1 = seq()

      val t2 = seq()

      liftAssert(ex1, t1, t2, expected = true)
    }

    scenario("Same transformations, intro Interface, intro package, move interface in new package "){
      def seq(pname : String) = ex1.seq(pname, ex1.packageMethodUsesViaThisField)

      val t1 = seq("p1")

      val t2 = seq("p2")

      liftAssert(ex1, t1, t2, expected = true)
    }

    scenario("Different transformations, intro Interface, intro (different) package, move interface in new package "){
      val t1 = ex1.seq("p1", ex1.packageMethodUsesViaThisField)

      val t2 = ex1.seq("p2", ex1.graph.rootId)

      liftAssert(ex1, t1, t2, expected = false)
    }

    val ex2 = new ExampleSample(puck.testExamplesPath + "/needToMergeInterfaces/A.java"){
      //val packageNeedToMergeInterfaces = fullName2id("examples.needToMergeInterfaces")

      val itcC = fullName2id("examples.needToMergeInterfaces.IC")
      val itcB = fullName2id("examples.needToMergeInterfaces.IB")

      def merge = TR.merge(graph, itcB, itcC)

      def mergeSwap = TR.merge(graph, itcC, itcB)

    }

    scenario("Same merge, same result "){
      val t1 = ex2.merge

      val t2 = ex2.merge

      liftAssert(ex2, t1, t2, expected = true)
    }

/*    def print(name : String)(graph : DependencyGraph): Unit ={
      import puck.graph.io._

      val options =
        PrintingOptions(VisibilitySet.allVisible(graph), printId = true)
        FilesHandler.makeImageFile(None, graph, JavaNode,
        options, puck.testExamplesPath + "/" + name)()
    }*/

    scenario("Different path, merge, same result "){
      ex1.logger = new PuckFileLogger({ case _ => true}, new java.io.File( puck.testExamplesPath + "/log"))

      val t1 =
        introItcPackageAndMove(ex1.classB, "p1", ex1.packageMethodUsesViaThisField)(ex1.graph)
        .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(ex1.classB, "p2", ex1.graph.rootId)(g)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge(g2, itcId, itcId2)}
        }
      val t2 =
        introItcPackageAndMove(ex1.classB, "p3", ex1.graph.rootId)(ex1.graph)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(ex1.classB, "p4", ex1.packageMethodUsesViaThisField)(g)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge(g2, itcId2, itcId)}
        }

      liftAssert(ex1, t1, t2, expected = true)
    }

  }

}
