
package puck.javaGraph

import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.nodeKind._
import puck.{Settings, PuckError, AcceptanceSpec}
import puck.javaGraph.JGraphUtils.{transformationRules => TR}

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
    val (p, g2) = TR.intro(g, pname, Package, None)
    (g2.addContains(pcontainer, p.id), p)
  }

  def introItcPackageAndMove(graph : GraphT, clazz : ConcreteNode, pname : String, pcontainer : NodeId) =
    graph.container(clazz.id) match {
      case None => throw new PuckError()
      case Some(classContainer) =>
        introInterface(graph, clazz, classContainer)

          .map { case (g, itc) => (introPackage(g, pname, pcontainer), itc)}

          .flatMap { case ((g, p), itc) => TR.move.typeDecl(g, itc.id, p.id) map ((_, p.id, itc.id))}
    }

  val methodUsesViaThisField = {
    val p = "methodUsesViaThisField"
    new ExampleSample(s"${Settings.testExamplesPath}/misc/$p/A.java") {
      val rootPackage = fullName2id(p)

      val classA = fullName2id(s"$p.A")
      val fieldA = fullName2id(s"$p.A.b")
      val methA = fullName2id(s"$p.A.ma__void")

      val classB = fullName2id(s"$p.B")
      val classBNode = graph.getConcreteNode(classB)
      val methB = fullName2id(s"$p.B.mb__void")

    }
  }
  val needToMergeInterfaces = {
    val p = "needToMergeInterfaces"
    new ExampleSample(s"${Settings.testExamplesPath}/misc/$p/A.java") {
      //val packageNeedToMergeInterfaces = fullName2id("examples.misc.needToMergeInterfaces")

      val itcC = fullName2id(s"$p.IC")
      val itcB = fullName2id(s"$p.IB")

    }
  }

  feature("Comparison"){

    def liftAssert(ex: ExampleSample, tg1: LoggedTG, tg2: LoggedTG, expected: Boolean): Unit = {
      val app = Applicative[LoggedTry]
      val res = app.apply2(tg1, tg2)(ex.compare)
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

      val t1 = TR.mergeInto(graph, itcC, itcB)
      val t2 = TR.mergeInto(graph, itcC, itcB)

      liftAssert(needToMergeInterfaces, t1, t2, expected = true)
    }

    scenario("Different path, merge, same result "){
      import methodUsesViaThisField._

      val t1 =
        introItcPackageAndMove(graph, classBNode, "p1", rootPackage)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, classBNode, "p2", graph.rootId)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.mergeInto(g2, itcId2, itcId)}
        }
      val t2 =
        introItcPackageAndMove(graph, classBNode, "p3", graph.rootId)
          .flatMap{ case (g, pid, itcId) =>
          introItcPackageAndMove(g, classBNode, "p4", rootPackage)
            .flatMap {case (g2, pid2, itcId2) =>
            TR.mergeInto(g2, itcId, itcId2)}
        }

      liftAssert(methodUsesViaThisField, t1, t2, expected = true)
    }


  }

}

