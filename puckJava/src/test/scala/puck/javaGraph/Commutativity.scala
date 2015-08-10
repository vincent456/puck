package puck.javaGraph

import java.io.File

import puck.graph.transformations.rules.Redirection
import puck.graph._
import puck.graph.comparison.Mapping
import puck.util.Debug
import puck.{QuickFrame, Settings, AcceptanceSpec}
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.nodeKind.Interface
import puck.javaGraph.JGraphUtils.{transformationRules => Rules, _}

class Commutativity extends AcceptanceSpec {

  def getDefinition(g : DependencyGraph, nid : NodeId) : NodeId =
    g.getConcreteNode(nid).definition(g).value

  val outDir = new File(Settings.tmpDir + "testPuck")

  feature("Commutativity - abstract") {
    val examplesPath = Settings.testExamplesPath + "/intro"

    val noSuperTypePath = examplesPath + "/interface/noExistingSuperType/"

    scenario("extract interface") {

      val _ = new ExampleSample(s"${noSuperTypePath}SimpleCase.java") {
        self =>

        val packageP = fullName2id("p")
        val classA = fullName2id("p.A")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right
        val g = g0.addContains(packageP, itc)

        val recompiledEx = self.applyChangeAndMakeExample(g, outDir)

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

    info("abstract class into interface - super type already present")
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

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }
  }

  feature("TypeMember uses redirection"){
    val examplesPath = Settings.testExamplesPath + "/redirection/"

    val typeMemberPath = examplesPath + "typeMember"
    scenario("From method to method superType"){
      val _ = new ExampleSample(s"$typeMemberPath/MethodToMethodSuperType.java") {
        val mUsed = fullName2id("p.Bimpl.m1__void")
        val mAbs = fullName2id("p.B.m1__void")
        val userDecl = fullName2id("p.A.m__void")

        val userDef = getDefinition(graph, userDecl)

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            Uses(userDef, mUsed),
            AccessAbstraction(mAbs, SupertypeAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }
  }

 }

