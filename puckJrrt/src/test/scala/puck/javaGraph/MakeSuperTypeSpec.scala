package puck.javaGraph

import puck.graph.Type
import puck.{Settings, AcceptanceSpec}

import puck.javaGraph.JGraphUtils.{transformationRules => TR}

class MakeSuperTypeSpec extends AcceptanceSpec {
  feature("Make super type") {
    val examplesPath = Settings.testExamplesPath + "/makeSuperType"
    val superInterfacePath = examplesPath + "/superInterface/"
    scenario("Compatible super interface") {
      val _ = new ScenarioFactory(s"$superInterfacePath/Compatible.java") {
        val classA = fullName2id("p.A")
        val superA = fullName2id("p.SuperA")

        val methInInterface = fullName2id("p.A.mInInterface()")
        val methNotInInterface = fullName2id("p.A.mNotInInterface()")
        val abstractMethInInterface = fullName2id("p.SuperA.mInInterface()")

        val g2 = TR.makeSuperType(graph, classA, superA)().right

        assert(g2.isa(classA, superA))

        g2.abstractions(methNotInInterface) should be(empty)

      }
    }

    scenario("Incompatible super interface") {
      val _ = new ScenarioFactory(s"$superInterfacePath/Incompatible.java") {
        val classA = fullName2id("p.A")
        val superA = fullName2id("p.SuperA")

        assertIsLeft( TR.makeSuperType(graph, classA, superA)(Type.errorOnImplemNotFound("A")))

      }
    }
  }

}