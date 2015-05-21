package puck.javaGraph

import puck.graph.constraints.SupertypeAbstraction
import puck.{Settings, AcceptanceSpec}

import puck.javaGraph.{JavaTransformationRules => TR}

class MakeSuperTypeSpec extends AcceptanceSpec {
  feature("Make super type") {
    val examplesPath = Settings.testExamplesPath + "/makeSuperType"
    val superInterfacePath = examplesPath + "/superInterface/"
    scenario("Compatible super interface") {
      val _ = new ExampleSample(s"$superInterfacePath/Compatible.java") {
        val classA = fullName2id("p.A")
        val superA = fullName2id("p.SuperA")

        val methInInterface = fullName2id("p.A.mInInterface__void")
        val methNotInInterface = fullName2id("p.A.mNotInInterface__void")
        val abstractMethInInterface = fullName2id("p.SuperA.mInInterface__void")

        val g2 = TR.makeSuperType(graph, classA, superA).value

        assert(g2.isa(classA, superA))

        g2.abstractions(methNotInInterface) should be(empty)

      }
    }

    scenario("Incompatible super interface") {
      val _ = new ExampleSample(s"$superInterfacePath/Incompatible.java") {
        val classA = fullName2id("p.A")
        val superA = fullName2id("p.SuperA")

        assertIsFailure( TR.makeSuperType(graph, classA, superA) )

      }
    }
  }

}