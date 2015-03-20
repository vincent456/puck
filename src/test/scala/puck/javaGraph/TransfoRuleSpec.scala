package puck
package javaGraph

import puck.graph.constraints.{AbstractionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.graph._
import puck.javaGraph.nodeKind._
import puck.util.{PuckSystemLogger, PuckFileLogger}

import scalaz.{Success, Failure}

import scala.language.reflectiveCalls
/**
 * Created by lorilan on 2/25/15.
 */
class TransfoRuleSpec extends AcceptanceSpec {

  /*    def print(name : String)(graph : DependencyGraph): Unit ={
      import puck.graph.io._

      val options =
        PrintingOptions(VisibilitySet.allVisible(graph), printId = true)
        FilesHandler.makeImageFile(None, graph, JavaNode,
        options, puck.testExamplesPath + "/" + name)()
    }*/


  info("Transformation Rules TestSuite")

  import Scenarii._

  implicit class TestValidation[G]( t : Try[G]){
    def assertSuccess(f : G => Unit) : Unit = {
      t match {
        case Failure(_) => assert(false)
        case Success(g) => f(g)
      }
    }  
  }

  implicit class TestOption[G]( t : Option[G]){
    def assertSome(f : G => Unit) : Unit = {
      t match {
        case None => assert(false)
        case Some(g) => f(g)
      }
    }
  }
  
  feature("Move"){
    val case1 = methodUsesViaThisField
    scenario("Move class"){
      val (g, package1) = introPackage(case1.graph, "p1", case1.rootPackage)
      TR.moveTo(g, case1.classB.id, package1.id).assertSuccess {
        g2 => g2.container(case1.classB.id).assertSome {
          pid0 => assert(pid0 == package1.id)
        }
      }
    }

    scenario("Move method used by this"){
      import usesThisMethod.{graph => initialGraph, _}

      val (classB, g2) = initialGraph.addConcreteNode("B", Class, NoType)
      g2.addContains(rootPackage, classB.id)

      TR.moveTo(g2, methMa2, classB.id).assertSuccess { g3 =>
        g3.container(methMa2).assertSome { cid =>
          assert(cid == classB.id)
        }
        assert(g3.uses(methMa1, methMa2))
      }
    }
  }

  feature("Redirection"){

    def classToAbsScenario
    ( graph : DependencyGraph,
      //typeUser : NodeId,
      typeMemberUser : NodeId,
      typeUsed : NodeId,
      typeMemberUsed : NodeId,
      newTypeUsed : NodeId,
      newTypeMemberUsed : NodeId,
      policy : AbstractionPolicy
      ) : Unit = {
      val typeUse = DGEdge.uses(typeMemberUser, typeUsed)
      assert(typeUse.exists(graph))
      assert(DGEdge.uses(typeMemberUser, typeMemberUsed).exists(graph))
      TR.redirectUsesOf(graph, typeUse, newTypeUsed, policy).assertSuccess {
        case (newTypeUse, g2) =>
          assert(DGEdge.uses(typeMemberUser, newTypeUsed) == newTypeUse)
          assert(newTypeUse.exists(g2))
          assert(DGEdge.uses(typeMemberUser, newTypeMemberUsed).exists(g2))
      }
    }

    ignore("From class to delegator class"){
      val ex = redirection.classToClassDelegate

      classToAbsScenario(ex.graph, ex.mUser,
        ex.delegatee, ex.mDelegatee,
        ex.delegator, ex.mDelegator,
        DelegationAbstraction)

      /*val use = DGEdge.uses(ex.mUser, ex.delegatee)
      assert(use.exists(ex.graph))
      assert(DGEdge.uses(ex.mUser, ex.mDelegatee).exists(ex.graph))
      TR.redirectUsesOf(ex.graph, use, ex.delegator, DelegationAbstraction) match {
        case Failure(_) => assert(false)
        case Success((newUse, g2)) =>
          assert(DGEdge.uses(ex.mUser, ex.delegator) == newUse)
          assert(newUse.exists(g2))
          assert(DGEdge.uses(ex.mUser, ex.mDelegator).exists(g2))
      }*/
    }

    scenario("From class to superType interface"){
      val ex = redirection.classToInterfaceSuperType

      val superTypeRegisteredAsClassUsedAbstraction =
        ex.graph.abstractions(ex.classUsed).exists{
          case ((absId, policy)) =>
            absId == ex.superType && policy == SupertypeAbstraction
        }
      assert(superTypeRegisteredAsClassUsedAbstraction)

      val use = DGEdge.uses(ex.mUser, ex.classUsed)
      assert(use.exists(ex.graph))
      assert(DGEdge.uses(ex.mUser, ex.mUsed).exists(ex.graph))
      TR.redirectUsesOf(ex.graph, use, ex.superType, SupertypeAbstraction) match {
        case Failure(_) => assert(false)
        case Success((newUse, g2)) =>
          quickFrame(g2)
          assert(DGEdge.uses(ex.mUser, ex.superType) == newUse)
          assert(newUse.exists(g2))
          assert(DGEdge.uses(ex.mUser, ex.absmUsed).exists(g2))
      }
    }

  }
}
