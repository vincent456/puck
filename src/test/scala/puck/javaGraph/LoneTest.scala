package puck.javaGraph

import puck._
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.Scenarii._
import puck.javaGraph.nodeKind.Interface
import puck.util.PuckSystemLogger

import scalaz.{Success, Failure}

/**
 * Created by lorilan on 3/30/15.
 */
class LoneTest extends AcceptanceSpec {

  def assertSuccess[G](t : Try[G])(f : G => Unit) : Unit = {
    t match {
      case Failure(_) => assert(false)
      case Success(g) => f(g)
    }
  }
  val examplesPath = puck.testExamplesPath + "/intro"

  scenario("Intro interface - no existing super type - field self use in class"){
    val p = "introInterfaceNoExistingSuperTypeWithSelfUse"
    val _ = new ExampleSample(s"$examplesPath/$p/B.java") {
      val classB = fullName2id(s"$p.B")
      val field = fullName2id(s"$p.B.f")

      val fieldUserThatShouldNotBeInInterface =
        fullName2id(s"$p.B.fieldUserThatShouldNotBeInInterface__B")

      assert( graph.directSuperTypes(classB).isEmpty )

      assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
      assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )

      assert( graph.abstractions(classB).isEmpty )
      assert( graph.abstractions(field).isEmpty )
      assert( graph.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty )
      puck.quickFrame(graph)
      assertSuccess(TR.createAbstraction(graph.withLogger(new PuckSystemLogger(_ => true)), graph.getConcreteNode(classB),
        Interface, SupertypeAbstraction)){
        case (itc, g) =>
          assert( g.isa(classB, itc.id) )

          g.abstractions(classB).size shouldBe 1
          assert( g.abstractions(field).isEmpty ,
            "Field cannot be exposed in an interface")
          assert( g.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty,
            "Method use concrete class field via parameter, should not be abstracted")

          assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
          assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )

      }
    }
  }
}
