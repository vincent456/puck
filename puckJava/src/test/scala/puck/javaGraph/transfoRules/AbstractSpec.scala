package puck.javaGraph.transfoRules

import puck.graph.AccessAbstraction
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.ExampleSample
import puck.javaGraph.JGraphUtils.{transformationRules => TR}
import puck.javaGraph.nodeKind.Interface
import puck.{AcceptanceSpec, GetDefinitionValue, Settings}

class AbstractSpec extends AcceptanceSpec with GetDefinitionValue {
  feature("Abstract class into interface"){
    val examplesPath = Settings.testExamplesPath + "/intro"

    info("no pre-existing super type")
    val noSuperTypePath = examplesPath + "/interface/noExistingSuperType"
    scenario("simple case"){
      val _ = new ExampleSample(s"$noSuperTypePath/SimpleCase.java") {
        val classA = fullName2id("p.A")
        val methM = fullName2id("p.A.m__void")
        val field = fullName2id("p.A.f")

        assert( graph.directSuperTypes(classA).isEmpty )

        assert( graph.abstractions(classA).isEmpty )
        assert( graph.abstractions(methM).isEmpty )
        assert( graph.abstractions(field).isEmpty )


        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right
        assert( g.isa(classA, itc) )

        g.abstractions(classA).size shouldBe 1
        g.abstractions(methM).size shouldBe 1
        assert( g.abstractions(field).isEmpty ,
          "Field cannot be exposed in an interface")
      }

    }

    scenario("method self use in class"){
      val _ = new ExampleSample(s"$noSuperTypePath/MethodSelfUse.java") {
        val classA = fullName2id("p.A")
        val methM = fullName2id("p.A.m__void")
        val methMUserDecl = fullName2id("p.A.methodUser__A")
        val theParam = fullName2id("p.A.methodUser__A.a")
        val methMUserDef = getDefinition(graph, methMUserDecl)

        assert( graph.directSuperTypes(classA).isEmpty )

        assert( graph.uses(theParam, classA) )
        assert( graph.uses(methMUserDef, methM) )

        assert( graph.abstractions(classA).isEmpty )
        assert( graph.abstractions(methM).isEmpty )
        assert( graph.abstractions(methMUserDecl).isEmpty )
        assert( graph.abstractions(methMUserDef).isEmpty )


        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right

        assert( g.isa(classA, itc) )

        g.abstractions(classA).size shouldBe 1
        g.abstractions(methM).size shouldBe 1
        g.abstractions(methMUserDecl).size shouldBe 1
        assert( g.abstractions(methMUserDef).isEmpty )


        val AccessAbstraction(methMAbs, _) = g.abstractions(methM).head

        assert( g.uses(theParam, itc) )
        assert( g.uses(methMUserDef, methMAbs) )
      }


    }

    scenario("field self use in class"){
      val _ = new ExampleSample(s"$noSuperTypePath/FieldSelfUse.java") {
        val classB = fullName2id("p.B")
        val field = fullName2id("p.B.f")

        val fieldUserThatShouldNotBeInInterfaceDecl =
          fullName2id(s"p.B.fieldUserThatShouldNotBeInInterface__B")
        val theParam =  fullName2id(s"p.B.fieldUserThatShouldNotBeInInterface__B.b")
        val fieldUserThatShouldNotBeInInterfaceDef =
          getDefinition(graph, fieldUserThatShouldNotBeInInterfaceDecl)

        assert( graph.directSuperTypes(classB).isEmpty )

        assert( graph.uses(theParam, classB) )
        assert( graph.uses(fieldUserThatShouldNotBeInInterfaceDef, field) )

        assert( graph.abstractions(classB).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatShouldNotBeInInterfaceDecl).isEmpty )


        assert( !TR.abstracter.canBeAbstracted(graph,
          graph.getConcreteNode(fieldUserThatShouldNotBeInInterfaceDecl),
          graph.getConcreteNode(classB),
          SupertypeAbstraction))

        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classB),
            Interface, SupertypeAbstraction).right
        assert( g.isa(classB, itc) )

        g.abstractions(classB).size shouldBe 1
        assert( g.abstractions(field).isEmpty , "Field cannot be exposed in an interface")
        assert( g.abstractions(fieldUserThatShouldNotBeInInterfaceDecl).isEmpty,
          "Method use concrete class field, should not be abstracted")

        assert( g.uses(theParam, classB) )
        assert( g.uses(fieldUserThatShouldNotBeInInterfaceDef, field) )


      }
    }

    scenario("field use via parameter of self type"){
      val _ = new ExampleSample(s"$noSuperTypePath/FieldUseViaParameterSelfType.java") {
        val classC = fullName2id("p.C")
        val field = fullName2id("p.C.f")

        val fieldUserThatCanBeInInterfaceDecl =
          fullName2id("p.C.fieldUserThatCanBeInInterface__void")

        val fieldUserThatCanBeInInterfaceDef =
          getDefinition(graph, fieldUserThatCanBeInInterfaceDecl)

        assert( graph.directSuperTypes(classC).isEmpty )

        //assert( !graph.uses(fieldUserThatCanBeInInterface, classC) )
        assert( graph.uses(fieldUserThatCanBeInInterfaceDef, field) )

        assert( graph.abstractions(classC).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatCanBeInInterfaceDecl).isEmpty )

        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classC),
            Interface, SupertypeAbstraction).right

        assert( g.isa(classC, itc) )

        g.abstractions(classC).size shouldBe 1
        assert( g.abstractions(field).isEmpty ,
          "Field cannot be exposed in an interface")
        g.abstractions(fieldUserThatCanBeInInterfaceDecl).size shouldBe 1

        //assert( graph.uses(fieldUserThatCanBeInInterface, classC) )
        assert( graph.uses(fieldUserThatCanBeInInterfaceDef, field) )

      }
    }

    scenario("use of type member sibling by self and parameter"){
      val _ = new ExampleSample(s"$noSuperTypePath/SelfTypeMemberUseViaParameterAndSelf.java"){
        val classA = fullName2id("p.A")
        val field = fullName2id("p.A.f")
        val usedMeth = fullName2id("p.A.m__int")

        val methCanBeInInterface = fullName2id("p.A.canBeInInterface__A")
        val methCannotBeInInterface = fullName2id("p.A.cannotBeInInterface__A")

        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right

        assert( g.isa(classA, itc))

        assert( g.abstractions(methCannotBeInInterface).isEmpty)
        g.abstractions(methCanBeInInterface).size shouldBe 1


      }
    }

    ignore("use of type member sibling by local variable and parameter"){
      val _ = new ExampleSample(s"$noSuperTypePath/SelfTypeMemberUseViaParameterAndLocalVariable.java"){
        val classA = fullName2id("p.A")
        val field = fullName2id("p.A.f")
        val usedMeth = fullName2id("p.A.m__int")

        val methCanBeInInterface = fullName2id("p.A.canBeInInterface__A")
        val methCannotBeInInterface = fullName2id("p.A.cannotBeInInterface__A")

        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right

        assert( g.isa(classA, itc))

        assert( g.abstractions(methCannotBeInInterface).isEmpty)
        g.abstractions(methCanBeInInterface).size shouldBe 1

      }
    }

    ignore("abstract class into interface - cyclic uses in class (recursion)"){}
    ignore("abstract class into interface - super interface existing"){}
    ignore("abstract class into interface - super class existing"){}


    info("super type already present")
    val withSuperTypePath = examplesPath + "/interface/existingSuperType"

    scenario("existing supertype - simple case"){
      val _ = new ExampleSample(s"$withSuperTypePath/SimpleCase.java") {
        val classA = fullName2id("p.A")
        val methInInterface = fullName2id("p.A.mInInterface__void")
        val methNotInInterface = fullName2id("p.A.mNotInInterface__void")

        val superA = fullName2id("p.SuperA")
        val absMethInInterface = fullName2id("p.SuperA.mInInterface__void")


        assert( graph.isa(classA, superA) )


        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right

        assert( g.isa(classA, itc) )
        assert( g.isa(itc, superA) )

        val absMeths = g.content(itc)
        val mInBothInterface  =
          absMeths.find(g.getConcreteNode(_).name == "mInInterface").value
        val mInNewInterface =
          absMeths.find(g.getConcreteNode(_).name == "mNotInInterface").value

        g.abstractions(methInInterface)  should contain (AccessAbstraction(mInBothInterface, SupertypeAbstraction))
        g.abstractions(mInBothInterface) should contain (AccessAbstraction(absMethInInterface, SupertypeAbstraction))
        g.abstractions(methNotInInterface)  should contain (AccessAbstraction(mInNewInterface, SupertypeAbstraction))

      }
    }
  }
}
