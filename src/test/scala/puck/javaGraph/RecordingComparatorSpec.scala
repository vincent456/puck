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

    val ex1 = methodUsesViaThisField

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
        .map {case (classBAbs, g) => g.addContains(ex1.rootPackage, classBAbs.id)}

      val t1 = seq()

      val t2 = seq()

      liftAssert(ex1, t1, t2, expected = true)
    }

    scenario("Same transformations, intro Interface, intro package, move interface in new package "){
      def seq(pname : String) =
        introItcPackageAndMove(ex1.graph, ex1.classB, pname, ex1.rootPackage) map (_._1)

      val t1 = seq("p1")
      val t2 = seq("p2")

      liftAssert(ex1, t1, t2, expected = true)
    }

    scenario("Different transformations, intro Interface, intro (different) package, move interface in new package "){
      val t1 = introItcPackageAndMove(ex1.graph, ex1.classB, "p1", ex1.rootPackage) map (_._1)
      val t2 = introItcPackageAndMove(ex1.graph, ex1.classB, "p2", ex1.graph.rootId) map (_._1)

      liftAssert(ex1, t1, t2, expected = false)
    }

    val ex2 = needToMergeInterfaces

    scenario("Same merge, same result "){
      val t1 = TR.merge(ex2.graph, ex2.itcB, ex2.itcC)


      val t2 = TR.merge(ex2.graph, ex2.itcB, ex2.itcC)

      liftAssert(ex2, t1, t2, expected = true)
    }

    scenario("Different path, merge, same result "){


      val t1 =
        introItcPackageAndMove(ex1.graph, ex1.classB, "p1", ex1.rootPackage)
        .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, ex1.classB, "p2", ex1.graph.rootId)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge(g2, itcId, itcId2)}
        }
      val t2 =
        introItcPackageAndMove(ex1.graph, ex1.classB, "p3", ex1.graph.rootId)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, ex1.classB, "p4", ex1.rootPackage)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge(g2, itcId2, itcId)}
        }

      liftAssert(ex1, t1, t2, expected = true)
    }

  }

}
