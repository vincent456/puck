
package puck.javaGraph

import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.nodeKind._
import puck.AcceptanceSpec

import scalaz._
/**
 * Created by lorilan on 22/02/15.
 */
import scalaz.Validation.FlatMap._


import scala.language.reflectiveCalls

class RecordingComparatorSpec extends AcceptanceSpec {

  info("A recording comparator should be able to tell if two refactoring plan are equal or not")

  import Scenarii._

  feature("Comparison"){

    def liftAssert(ex: ExampleSample, tg1: TryG, tg2: TryG, expected: Boolean): Unit = {
      val app = Applicative[puck.graph.Try]
      val res = app.apply2(tg1, tg2)(ex.compare)
      res match {
        case Success(b) => assert(b === expected)
        case Failure(_) => assert(false)
      }
    }

    scenario("Same transformations, intro Interface") {
      import methodUsesViaThisField._

      def seq() = TR.createAbstraction(graph, classBNode, Interface, SupertypeAbstraction)
        .map { case (classBAbs, g) => g.addContains(rootPackage, classBAbs.id) }

      val t1 = seq()

      val t2 = seq()

      liftAssert(methodUsesViaThisField, t1, t2, expected = true)
    }

    scenario("Same transformations, intro Interface, intro package, move interface in new package ") {
      import methodUsesViaThisField._

      def seq(pname: String) =
        introItcPackageAndMove(graph, classBNode, pname, rootPackage) map (_._1)

      val t1 = seq("p1")
      val t2 = seq("p2")

      liftAssert(methodUsesViaThisField, t1, t2, expected = true)
    }

    scenario("Different transformations, intro Interface, intro (different) package, move interface in new package ") {
      import methodUsesViaThisField._

      val t1 = introItcPackageAndMove(graph, classBNode, "p1", rootPackage) map (_._1)
      val t2 = introItcPackageAndMove(graph, classBNode, "p2", graph.rootId) map (_._1)

      liftAssert(methodUsesViaThisField, t1, t2, expected = false)
    }

    scenario("Same merge, same result ") {
      import needToMergeInterfaces._

      val t1 = TR.merge(graph, itcB, itcC)
      val t2 = TR.merge(graph, itcB, itcC)

      liftAssert(needToMergeInterfaces, t1, t2, expected = true)
    }

    scenario("Different path, merge, same result "){
      import methodUsesViaThisField._

      val t1 =
        introItcPackageAndMove(graph, classBNode, "p1", rootPackage)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, classBNode, "p2", graph.rootId)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge(g2, itcId, itcId2)}
        }
      val t2 =
        introItcPackageAndMove(graph, classBNode, "p3", graph.rootId)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, classBNode, "p4", rootPackage)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge(g2, itcId2, itcId)}
        }

      liftAssert(methodUsesViaThisField, t1, t2, expected = true)
    }


  }

}

