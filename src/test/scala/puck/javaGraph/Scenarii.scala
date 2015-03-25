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

  val methodUsesViaThisField = new ExampleSample(puck.testExamplesPath + "/methodUsesViaThisField/A.java"){
    val rootPackage = fullName2id("examples.methodUsesViaThisField")

    val classA = fullName2id("examples.methodUsesViaThisField.A")
    val fieldA = fullName2id("examples.methodUsesViaThisField.A.b")
    val methA = fullName2id("examples.methodUsesViaThisField.A.ma__void")

    val classB = fullName2id("examples.methodUsesViaThisField.B")
    val classBNode = graph.getConcreteNode(classB)
    val methB = fullName2id("examples.methodUsesViaThisField.B.mb__void")

  }

  val needToMergeInterfaces = new ExampleSample(puck.testExamplesPath + "/needToMergeInterfaces/A.java"){
    //val packageNeedToMergeInterfaces = fullName2id("examples.needToMergeInterfaces")

    val itcC = fullName2id("examples.needToMergeInterfaces.IC")
    val itcB = fullName2id("examples.needToMergeInterfaces.IB")

  }

  val graphBuildingExamplesPath = puck.testExamplesPath + "/graphBuilding/"

  object typeRelationship{
    val examplesPath = graphBuildingExamplesPath +  "typeRelationship/"

    val callOnField = new ExampleSample(examplesPath + "/callOnField/A.java"){

      val fieldTypeUser = fullName2id("callOnField.A.b")
      val methUser = fullName2id("callOnField.A.ma__void")

      val typeUsed = fullName2id("callOnField.B")
      val typeMemberUsed = fullName2id("callOnField.B.mb__void")

    }

    val chainedCall = new ExampleSample(examplesPath + "chainedCall/A.java"){
      val mUser = fullName2id("chainedCall.A.ma__void")
      val mUsed = fullName2id("chainedCall.C.mc__void")
      val mIntermediate = fullName2id("chainedCall.B.mb__void")
      val classUsed = fullName2id("chainedCall.C")
    }

    val callOnLocalVariable = new ExampleSample(examplesPath + "callOnLocalVariable/A.java"){

      val mUser = fullName2id("callOnLocalVariable.A.ma__void")
      val mUsed = fullName2id("callOnLocalVariable.B.mb__void")

      val classUsed = fullName2id("callOnLocalVariable.B")
    }

    val callOnParameter = new ExampleSample(examplesPath + "callOnParameter/A.java"){

      val mUser = fullName2id("callOnParameter.A.ma__B")
      val classUsed = fullName2id("callOnParameter.B")
      val mUsed = fullName2id("callOnParameter.B.mb__void")
    }

  }

  object abstractionRegistration {
    val examplesPath = graphBuildingExamplesPath+  "abstractionRegistration/"

    val interfaceSupertype = new ExampleSample(examplesPath + "interfaceSupertype/A.java") {

      val classUsed = fullName2id("interfaceSupertype.A")
      val mUsed = fullName2id("interfaceSupertype.A.ma__void")
      val superType = fullName2id("interfaceSupertype.SuperType")
      val absmUsed = fullName2id("interfaceSupertype.SuperType.ma__void")

    }
  }

}
