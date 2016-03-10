/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */


package puck.javaGraph

import puck.graph.constraints.SupertypeAbstraction
import puck.graph._
import puck.javaGraph.nodeKind._
import puck.{Settings, PuckError, AcceptanceSpec}
import puck.jastadd.ExtendJGraphUtils.{transformationRules => TR}

import scalaz._


class RecordingComparatorSpec extends AcceptanceSpec {

  info("A recording comparator should be able to tell if two refactoring plan are equal or not")

  type TryG = puck.graph.Try[DependencyGraph]

  type GraphT = DependencyGraph

  def introInterface(g : GraphT, clazz : ConcreteNode, pcontainer : NodeId) : LoggedTry[(GraphT, ConcreteNode)]= {
    TR.abstracter.createAbstraction(g, clazz, Interface, SupertypeAbstraction)
      .map {case (AccessAbstraction(classAbs, _), g) =>
        (g.addContains(pcontainer, classAbs), g.getConcreteNode(classAbs))
        case _ =>
          assert(false)
          sys.error("should not happen")
    }

  }

  def introPackage(g : GraphT, pname : String, pcontainer : NodeId) : (GraphT, ConcreteNode) = {
    val (p, g2) = TR.intro(g, pname, Package)
    (g2.addContains(pcontainer, p.id), p)
  }

  def introItcPackageAndMove(graph : GraphT, clazz : ConcreteNode, pname : String, pcontainer : NodeId) =
    graph.container(clazz.id) match {
      case None => throw new PuckError()
      case Some(classContainer) =>
        introInterface(graph, clazz, classContainer)

          .map { case (g, itc) => (introPackage(g, pname, pcontainer), itc)}

          .flatMap { case ((g, p), itc) => TR.move.staticDecl(g, itc.id, p.id) map ((_, p.id, itc.id))}
    }

  val methodUsesViaThisField = {
    val p = "methodUsesViaThisField"
    new ScenarioFactory(s"${Settings.testExamplesPath}/misc/$p/A.java") {
      val rootPackage = fullName2id(p)

      val classA = fullName2id(s"$p.A")
      val fieldA = fullName2id(s"$p.A.b")
      val methA = fullName2id(s"$p.A.ma()")

      val classB = fullName2id(s"$p.B")
      val classBNode = graph.getConcreteNode(classB)
      val methB = fullName2id(s"$p.B.mb()")

    }
  }
  val needToMergeInterfaces = {
    val p = "needToMergeInterfaces"
    new ScenarioFactory(s"${Settings.testExamplesPath}/misc/$p/A.java") {
      //val packageNeedToMergeInterfaces = fullName2id("examples.misc.needToMergeInterfaces")

      val itcC = fullName2id(s"$p.IC")
      val itcB = fullName2id(s"$p.IB")

    }
  }

  feature("Comparison"){

    def liftAssert(ex: ScenarioFactory, tg1: LoggedTG, tg2: LoggedTG, expected: Boolean): Unit = {
      val app = Applicative[LoggedTry].lift2(ex.compare)
      val res = app(tg1, tg2)

      res.value match {
        case \/-(b) => assert(b === expected)
        case -\/(_) => assert(false)
      }
    }

    scenario("Same transformations, intro Interface") {
      import methodUsesViaThisField._

      def seq() = TR.abstracter.createAbstraction(graph, classBNode, Interface, SupertypeAbstraction)
        .map { case (AccessAbstraction(classBAbs, _), g) => g.addContains(rootPackage, classBAbs)
              case _ =>
                assert(false)
                sys.error("should not happen")}

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

      val t1 = TR.merge.mergeInto(graph, itcC, itcB)
      val t2 = TR.merge.mergeInto(graph, itcC, itcB)

      liftAssert(needToMergeInterfaces, t1, t2, expected = true)
    }

    scenario("Different path, merge, same result "){
      import methodUsesViaThisField._

      val t1 =
        introItcPackageAndMove(graph, classBNode, "p1", rootPackage)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, classBNode, "p2", graph.rootId)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge.mergeInto(g2, itcId2, itcId)}
        }
      val t2 =
        introItcPackageAndMove(graph, classBNode, "p3", graph.rootId)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, classBNode, "p4", rootPackage)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.merge.mergeInto(g2, itcId, itcId2)}
        }

      liftAssert(methodUsesViaThisField, t1, t2, expected = true)
    }


  }

}

