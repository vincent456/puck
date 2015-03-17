package puck.javaGraph

import puck.PuckError
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.{JavaTransformationRules => TR}
import puck.javaGraph.nodeKind.{Package, Interface}

import scalaz.Validation.FlatMap._
/**
 * Created by lorilan on 2/25/15.
 */

import scala.language.reflectiveCalls

object Scenarii {
  type TryG = puck.graph.Try[DependencyGraph]

  type GraphT = DependencyGraph

  def introInterface(g : GraphT, clazz : ConcreteNode, pcontainer : NodeId) : Try[(GraphT, ConcreteNode)]= {
    TR.createAbstraction(g, clazz, Interface, SupertypeAbstraction)
      .map {case (classAbs, g) =>
      (g.addContains(pcontainer, classAbs.id), classAbs)}

  }

  def introPackage(g : GraphT, pname : String, pcontainer : NodeId) : (GraphT, ConcreteNode) = {
    val (p, g2) = g.addConcreteNode(pname, Package, NoType)
    (g2.addContains(pcontainer, p.id), p)
  }

  def introItcPackageAndMove(graph : GraphT, clazz : ConcreteNode, pname : String, pcontainer : NodeId) =
    graph.container(clazz.id) match {
      case None => throw new PuckError()
      case Some(classContainer) =>
        introInterface(graph, clazz, classContainer)

          .map { case (g, itc) => (introPackage(g, pname, pcontainer), itc)}

          .flatMap { case ((g, p), itc) => TR.moveTo(g, itc.id, p.id) map ((_, p.id, itc.id))}
    }

  val methodUsesViaThisField = new ExampleSample(puck.testExamplesPath + "/methodUsesViaThisField/A.java"){
    val rootPackage = fullName2id("examples.methodUsesViaThisField")

    val classA = fullName2id("examples.methodUsesViaThisField.A")
    val classB = graph.getConcreteNode(fullName2id("examples.methodUsesViaThisField.B"))

  }

  val needToMergeInterfaces = new ExampleSample(puck.testExamplesPath + "/needToMergeInterfaces/A.java"){
    //val packageNeedToMergeInterfaces = fullName2id("examples.needToMergeInterfaces")

    val itcC = fullName2id("examples.needToMergeInterfaces.IC")
    val itcB = fullName2id("examples.needToMergeInterfaces.IB")

  }

  val usesThisMethod = new ExampleSample(puck.testExamplesPath + "/usesThisMethod/A.java"){

    val rootPackage = fullName2id("examples.usesThisMethod")

    val classA = fullName2id("examples.usesThisMethod.A")
    val methMa1 = fullName2id("examples.usesThisMethod.A.ma1__void")
    val methMa2 = fullName2id("examples.usesThisMethod.A.ma2__void")

  }

}
