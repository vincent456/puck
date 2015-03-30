package puck.javaGraph

import puck.PuckError
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.nodeKind.{Package, Interface}
import puck.javaGraph.transformations.JavaTransformationRules

import scalaz.Validation.FlatMap._
/**
 * Created by lorilan on 2/25/15.
 */

import scala.language.reflectiveCalls

object Scenarii {

  type TryG = puck.graph.Try[DependencyGraph]

  type GraphT = DependencyGraph

  val TR = JavaTransformationRules


  def introInterface(g : GraphT, clazz : ConcreteNode, pcontainer : NodeId) : Try[(GraphT, ConcreteNode)]= {
    TR.createAbstraction(g, clazz, Interface, SupertypeAbstraction)
      .map {case (classAbs, g) =>
      (g.addContains(pcontainer, classAbs.id), classAbs)}

  }

  def introPackage(g : GraphT, pname : String, pcontainer : NodeId) : (GraphT, ConcreteNode) = {
    val (p, g2) = g.addConcreteNode(pname, Package, None)
    (g2.addContains(pcontainer, p.id), p)
  }

  def introItcPackageAndMove(graph : GraphT, clazz : ConcreteNode, pname : String, pcontainer : NodeId) =
    graph.container(clazz.id) match {
      case None => throw new PuckError()
      case Some(classContainer) =>
        introInterface(graph, clazz, classContainer)

          .map { case (g, itc) => (introPackage(g, pname, pcontainer), itc)}

          .flatMap { case ((g, p), itc) => TR.moveTypeDecl(g, itc.id, p.id) map ((_, p.id, itc.id))}
    }

  val methodUsesViaThisField = {
    val p = "methodUsesViaThisField"
    new ExampleSample(s"${puck.testExamplesPath}/misc/$p/A.java") {
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
    new ExampleSample(s"${puck.testExamplesPath}/misc/$p/A.java") {
      //val packageNeedToMergeInterfaces = fullName2id("examples.misc.needToMergeInterfaces")

      val itcC = fullName2id(s"$p.IC")
      val itcB = fullName2id(s"$p.IB")

    }
  }
}
