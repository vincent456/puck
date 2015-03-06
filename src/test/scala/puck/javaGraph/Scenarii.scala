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

  def introInterface(g : GraphT, classId : NodeId, pcontainer : NodeId) : Try[(GraphT, NodeId)]= {
    TR.createAbstraction(g, classId, Interface, SupertypeAbstraction)
      .map {case (classBAbsId, g) =>
      (g.addContains(pcontainer, classBAbsId), classBAbsId)}

  }

  def introPackage(g : GraphT, pname : String, pcontainer : NodeId) : (GraphT, NodeId) = {
    val (pid, g2) = g.addConcreteNode(pname, Package, NoType)
    (g2.addContains(pcontainer, pid), pid)
  }

  def introItcPackageAndMove(graph : GraphT, classId : NodeId, pname : String, pcontainer : NodeId) =
    graph.container(classId) match {
      case None => throw new PuckError()
      case Some(classContainer) =>
        introInterface(graph, classId, classContainer)

          .map { case (g, itcId) => (introPackage(g, pname, pcontainer), itcId)}

          .flatMap { case ((g, pid), itcId) => TR.moveTo(g, itcId, pid) map ((_, pid, itcId))}
    }

  val methodUsesViaThisField = new ExampleSample(puck.testExamplesPath + "/methodUsesViaThisField/A.java"){
    val rootPackage = fullName2id("examples.methodUsesViaThisField")

    val classA = fullName2id("examples.methodUsesViaThisField.A")
    val classB = fullName2id("examples.methodUsesViaThisField.B")

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
