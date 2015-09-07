package puck.javaGraph.commutativity


import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.Redirection
import puck.javaGraph.ExampleSample
import puck.javaGraph.JGraphUtils.{transformationRules => Rules}
import puck.javaGraph.nodeKind.Interface
import puck.{AcceptanceSpec, QuickFrame, Settings}
import puck.Settings.outDir
class CommutativityAbstract extends AcceptanceSpec {

  val examplesPath = Settings.testExamplesPath + "/intro"

  feature("Abstract class into interface") {

    info("no pre-existing super type")
    val noSuperTypePath = examplesPath + "/interface/noExistingSuperType/"

    scenario("simple case") {

      val _ = new ExampleSample(s"${noSuperTypePath}SimpleCase.java") {

        val packageP = fullName2id("p")
        val classA = fullName2id("p.A")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right
        val g = g0.addContains(packageP, itc)

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }

    }

    scenario("method self use in class"){
      val _ = new ExampleSample(s"$noSuperTypePath/MethodSelfUse.java") {

        val packageP = fullName2id("p")
        val classA = fullName2id("p.A")


        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right

        val g = g0.addContains(packageP, itc)

        //println(Debug.showNodeIndex.shows(g.nodesIndex))

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("field self use in class"){
      val _ = new ExampleSample(s"$noSuperTypePath/FieldSelfUse.java") {
        val packageP = fullName2id("p")
        val classB = fullName2id("p.B")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classB),
            Interface, SupertypeAbstraction).right
        val g = g0.addContains(packageP, itc)


        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("field use via parameter of self type"){
      val _ = new ExampleSample(s"$noSuperTypePath/FieldUseViaParameterSelfType.java") {
        val packageP = fullName2id("p")
        val classC = fullName2id("p.C")


        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classC),
            Interface, SupertypeAbstraction).right
        val g = g0.addContains(packageP, itc)

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("use of type member sibling by self and parameter"){
      val _ = new ExampleSample(s"$noSuperTypePath/SelfTypeMemberUseViaParameterAndSelf.java"){
        val packageP = fullName2id("p")
        val classA = fullName2id("p.A")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right
        val g = g0.addContains(packageP, itc)

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    info("super type already present")
    val withSuperTypePath = examplesPath + "/interface/existingSuperType"

    scenario("existing supertype - simple case"){
      val _ = new ExampleSample(s"$withSuperTypePath/SimpleCase.java") {
        val packageP = fullName2id("p")
        val classA = fullName2id("p.A")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right
        val g = g0.addContains(packageP, itc)

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

//        QuickFrame(g, "g")
//        QuickFrame(recompiledEx.graph, "recompiled")
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }
  }



}

