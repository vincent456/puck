package puck
package javaGraph

import puck.graph.{DGEdge, NoType}
import puck.javaGraph.nodeKind._
import puck.javaGraph.{JavaTransformationRules => TR}
import puck.util.PuckFileLogger

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

  feature("Move"){
    val case1 = methodUsesViaThisField
    scenario("Move class"){
      val (g, p) = introPackage(case1.graph, "p1", case1.rootPackage)
      val tryG = TR.moveTo(g, case1.classB.id, p.id)

      tryG match{
        case Failure(_) => assert(false)
        case Success(g2) =>
          g2.container(case1.classB.id) match {
            case None => assert(false)
            case Some(pid0) => assert(pid0 == p.id)
          }
      }
    }

    scenario("Move method used by this"){
      import usesThisMethod.{graph => initialGraph, _}

      val logger = new PuckFileLogger({ case _ => true}, new java.io.File( puck.testExamplesPath + "/log"))

      val (classB, g2) = initialGraph.newGraph(nLogger = logger).addConcreteNode("B", Class, NoType)
      g2.addContains(rootPackage, classB.id)

      println("initialGraph.uses(classA, classA) = " + initialGraph.uses(classA, classA))
      println("initialGraph.typeUsesOf(DGEdge.uses(methMa1, methMa2)) = " + initialGraph.typeUsesOf(DGEdge.uses(methMa1, methMa2)))

      val tryG = TR.moveTo(g2, methMa2, classB.id)

      tryG match{
        case Failure(_) => assert(false)
        case Success(g3) =>

          g3.container(methMa2) match {
            case None => assert(false)
            case Some(cid) => assert(cid == classB.id)
          }

          assert(g3.uses(methMa1, methMa2))
      }
    }
  }

}
