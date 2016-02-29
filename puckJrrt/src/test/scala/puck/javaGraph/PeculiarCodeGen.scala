package puck.javaGraph

import puck.graph.comparison.Mapping
import puck.{Settings, AcceptanceSpec}

/**
  * Created by lorilan on 04/02/16.
  */
class PeculiarCodeGen extends AcceptanceSpec {
  val examplesPath = Settings.testExamplesPath + "/codeGen"

  feature("Parse lock unlock gen") {

    def makeTest(f : String) : Unit = {
      val _ = new ScenarioFactory(f) {
        val recompiledEx = applyChangeAndMakeExample(graph, Settings.outDir)
        assert( Mapping.equals(graph, recompiledEx.graph) )
      }
    }

    info("tests generated by finding samples of code that required debugging")

    scenario("Anonymous class") {
      makeTest(s"$examplesPath/anonymousClass/Test.java")
    }

    scenario("enum") {
      makeTest(s"$examplesPath/enum/Test.java")
    }

    scenario("inner enum") {
      makeTest(s"$examplesPath/innerEnum/Test.java")
    }

    scenario("method call on anonymous class instantiation") {
      makeTest(s"$examplesPath/anonymousClassInstCall/Test.java")
    }
    scenario("empty interface with comments") {
      makeTest(s"$examplesPath/emptyInterfaceWithComment/Test.java")
    }

    scenario("gen type parameterized with an up-bounded wildcard") {
      makeTest(s"$examplesPath/genTypeUpBound/Test.java")
    }

    scenario("instance initializer") {
      makeTest(s"$examplesPath/instanceInit/Test.java")
    }

    scenario("parameterized class instanciation") {
      makeTest(s"$examplesPath/parClassInstanciation/Test.java")
    }

    scenario("parameterized class subtyping ") {
      makeTest(s"$examplesPath/parClassSubtyping/Test.java")
    }

    scenario("chained call with more than one argument") {
      makeTest(s"$examplesPath/chainedCallWithArgs/Test.java")
    }

    scenario("overloading with variadic method"){
      makeTest(s"${Settings.testExamplesPath}/graphBuilding/variadicMethod/A.java")
    }

    scenario("wild card usage") {
      makeTest(s"$examplesPath/wild/Test.java")
    }
  }

}
