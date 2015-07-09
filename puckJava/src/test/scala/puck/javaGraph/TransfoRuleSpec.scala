package puck
package javaGraph


import puck.graph.transformations.rules.Redirection
import puck.graph.{Uses, DGEdge, AccessAbstraction}
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.javaGraph.nodeKind._
import puck.javaGraph.JGraphUtils.{transformationRules => TR}


class TransfoRuleSpec extends AcceptanceSpec {

  feature("Abstract"){
    val examplesPath = Settings.testExamplesPath + "/intro"

    info("Abstract class with interface -- no pre-existing super type")
    val noSuperTypePath = examplesPath + "/interface/noExistingSuperType"
    scenario("no existing super type - simple case"){
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
        val methMUser = fullName2id("p.A.methodUser__A")

        assert( graph.directSuperTypes(classA).isEmpty )

        assert( graph.uses(methMUser, classA) )
        assert( graph.uses(methMUser, methM) )

        assert( graph.abstractions(classA).isEmpty )
        assert( graph.abstractions(methM).isEmpty )
        assert( graph.abstractions(methMUser).isEmpty )


        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).right

        assert( g.isa(classA, itc) )

        g.abstractions(classA).size shouldBe 1
        g.abstractions(methM).size shouldBe 1
        g.abstractions(methMUser).size shouldBe 1

        val AccessAbstraction(methMAbs, _) = g.abstractions(methM).head

        assert( g.uses(methMUser, methMAbs) )
        assert( g.uses(methMUser, itc) )
      }


    }

    scenario("field self use in class"){
      val _ = new ExampleSample(s"$noSuperTypePath/FieldSelfUse.java") {
        val classB = fullName2id("p.B")
        val field = fullName2id("p.B.f")

        val fieldUserThatShouldNotBeInInterface =
          fullName2id(s"p.B.fieldUserThatShouldNotBeInInterface__B")

        assert( graph.directSuperTypes(classB).isEmpty )

        assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
        assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )

        assert( graph.abstractions(classB).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty )


        assert( !TR.abstracter.canBeAbstracted(graph,
          graph.getConcreteNode(fieldUserThatShouldNotBeInInterface),
          graph.getConcreteNode(classB),
          SupertypeAbstraction))

        val (AccessAbstraction(itc, _), g) =
            TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classB),
              Interface, SupertypeAbstraction).right
        assert( g.isa(classB, itc) )

        g.abstractions(classB).size shouldBe 1
        assert( g.abstractions(field).isEmpty , "Field cannot be exposed in an interface")
        assert( g.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty,
                "Method use concrete class field, should not be abstracted")

        assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
        assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )


      }
    }

    scenario("field use via parameter of self type"){
      val _ = new ExampleSample(s"$noSuperTypePath/FieldUseViaParameterSelfType.java") {
        val classC = fullName2id("p.C")
        val field = fullName2id("p.C.f")

        val fieldUserThatCanBeInInterface =
          fullName2id("p.C.fieldUserThatCanBeInInterface__void")

        assert( graph.directSuperTypes(classC).isEmpty )

        //assert( !graph.uses(fieldUserThatCanBeInInterface, classC) )
        assert( graph.uses(fieldUserThatCanBeInInterface, field) )

        assert( graph.abstractions(classC).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatCanBeInInterface).isEmpty )

        val (AccessAbstraction(itc, _), g) =
          TR.abstracter.createAbstraction(graph, graph.getConcreteNode(classC),
            Interface, SupertypeAbstraction).right

        assert( g.isa(classC, itc) )

        g.abstractions(classC).size shouldBe 1
        assert( g.abstractions(field).isEmpty ,
          "Field cannot be exposed in an interface")
        g.abstractions(fieldUserThatCanBeInInterface).size shouldBe 1

        //assert( graph.uses(fieldUserThatCanBeInInterface, classC) )
        assert( graph.uses(fieldUserThatCanBeInInterface, field) )

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

    ignore("Intro interface - cyclic uses in class (recursion)"){}
    ignore("Intro interface - super interface existing"){}
    ignore("Intro interface - super class existing"){}


    info("Intro interface - super type already present")
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

        assert( g.uses(classA, itc) )
        assert( g.uses(itc, superA) )

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



  feature("Redirection"){

    val examplesPath = Settings.testExamplesPath + "/redirection/"
    val typeDeclPath = examplesPath + "typeDecl/"

    info("TypeDecl uses redirection")

    scenario("From class to superType interface"){
      val p = "classToInterfaceSuperType"
      val _ = new ExampleSample(s"$typeDeclPath/$p/A.java"){
        val mUser = fullName2id(s"$p.A.mUser__ClassUsed")

        val classUsed = fullName2id(s"$p.ClassUsed")
        val mUsed = fullName2id(s"$p.ClassUsed.mUsed__void")
        val superType = fullName2id(s"$p.SuperType")
        val absmUsed = fullName2id(s"$p.SuperType.mUsed__void")

        val typeUse = DGEdge.UsesK(mUser, classUsed)
        assert(typeUse.existsIn(graph))
        assert(DGEdge.UsesK(mUser, mUsed).existsIn(graph))

        val g2 =
          Redirection.redirectUsesAndPropagate(graph,
            typeUse, superType, SupertypeAbstraction, keepOldUse = false).right

        assert(DGEdge.UsesK(mUser, superType).existsIn(g2))
        assert(DGEdge.UsesK(mUser, absmUsed).existsIn(g2))

      }
    }

    //val classToClassSupertype

    //val interfaceToInterfaceSuperType

    ignore("From class to delegator class"){
      val p = "classToClassDelegate"
      new ExampleSample(s"$typeDeclPath/$p/A.java"){
        val mUser = fullName2id(s"$p.A.mUser__Delegatee")
        val delegatee = fullName2id(s"$p.Delegatee")
        val mDelegatee = fullName2id(s"$p.Delegatee.mUsed__void")

        val delegator = fullName2id(s"$p.Delegator")
        val mDelegator = fullName2id(s"$p.Delegator.mUsed__void")

        val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
                     .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

        val typeUse = DGEdge.UsesK(mUser, delegatee)
        assert(typeUse.existsIn(graph))
        assert(DGEdge.UsesK(mUser, mDelegatee).existsIn(graph))


        val g2 =
          Redirection.redirectUsesAndPropagate(graph,
            typeUse, delegator, DelegationAbstraction, keepOldUse = false).right

        assert(DGEdge.UsesK(mUser, delegator).existsIn(g2))
        assert(DGEdge.UsesK(mUser, mDelegator).existsIn(g2))
      };()

    }

    /*val interfaceToClassDelegate = new ExampleSample(typeDeclPath + "interfaceToClassDelegate/A.java"){
      val rootPackage = fullName2id("interfaceToClassDelegate")
      val mUser = fullName2id("interfaceToClassDelegate.A.mUser__I")
      val interface = fullName2id("interfaceToClassDelegate.I")
      val delegator = fullName2id("interfaceToClassDelegate.Delegator")
    }*/

    info("TypeConstructor uses redirection")

    val typeCtorPath = examplesPath + "typeConstructor"

    ignore("From constructor to constructorMethod hosted elsewhere - non static"){
      val p = "constructorToConstructorMethodHostedElsewhere"
      val _ = new ExampleSample(s"$typeCtorPath/$p/A.java"){
        val ctor = fullName2id(s"$p.B.B#_void")
        val ctorMethod = fullName2id(s"$p.Factory.createB__void")
        val factoryClass = fullName2id(s"$p.Factory")
        val factoryCtor = fullName2id(s"$p.Factory.Factory#_void")

        val caller = fullName2id(s"$p.A.m__void")

        val ctorUse = DGEdge.UsesK(caller, ctor)
        assert( ctorUse.existsIn(graph) )

        val ctorMethodUse =DGEdge.UsesK(caller, ctorMethod)
        assert( ! ctorMethodUse.existsIn(graph))

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            ctorUse, ctorMethod, DelegationAbstraction, keepOldUse = false).right

        assert( ctorMethodUse.existsIn(g2))
        assert( ! ctorUse.existsIn(g2) )
        assert(g2.uses(caller, factoryClass))
        //??
        assert(g2.uses(caller, factoryCtor))

      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static"){
      val p = "constructorToConstructorMethodHostedBySelf"
      val _ = new ExampleSample(s"$typeCtorPath/$p/A.java"){
        val ctor = fullName2id(s"$p.B.B#_void")
        val ctorMethod = fullName2id(s"$p.B.create__void")
        val constructedClass = fullName2id(s"$p.B")
        val caller = fullName2id(s"$p.A.m__void")
        val userOfTheCaller = fullName2id(s"$p.C.mc__void")

        val constructedClassUse = DGEdge.UsesK(caller, constructedClass)
        val ctorUse = DGEdge.UsesK(caller, ctor)
        assert( ctorUse existsIn graph )

        val ctorMethodUse = DGEdge.UsesK(caller, ctorMethod)
        assert( ! (ctorMethodUse existsIn graph))

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            ctorUse, ctorMethod, DelegationAbstraction, keepOldUse = false).right

        assert( ctorMethodUse existsIn g2)
        assert( !(ctorUse existsIn g2) )
        assert( constructedClassUse existsIn g2)
        assert( g2.uses(userOfTheCaller, ctor) )
        assert( g2.uses(userOfTheCaller, constructedClass) )

      }
    }

    info("TypeMember uses redirection")

    val typeMemberPath = examplesPath + "typeMember"

    scenario("From method to method superType"){
      val p = "methodToMethodSuperType"
      val _ = new ExampleSample(s"$typeMemberPath/$p/A.java") {
        val mUsed = fullName2id(s"$p.Bimpl.m1__void")
        val mAbs = fullName2id(s"$p.B.m1__void")
        val cUsed = fullName2id(s"$p.Bimpl")
        val cUsedCtor = fullName2id(s"$p.Bimpl.Bimpl#_void")

        val otherMused = fullName2id(s"$p.Bimpl.m2__void")
        val otherMabs = fullName2id(s"$p.B.m2__void")
        val cAbs = fullName2id(s"$p.B")

        val user =  fullName2id(s"$p.A.m__void")

        val useOfImplClass = Uses(user, cUsed)
        val useOfctor = Uses(user, cUsedCtor)
        val useOfmeth = Uses(user, mUsed)
        val useOfOtherMeth = Uses(user, otherMused)

        val useOfAbsClass = Uses(user, cAbs)
        val useOfmethAbs = Uses(user, mAbs)
        val useOfOtherMethAbs = Uses(user, otherMabs)

        assert(useOfImplClass existsIn graph)
        assert(useOfctor existsIn graph)
        assert(useOfmeth existsIn graph)
        assert(useOfOtherMeth existsIn graph)

        assert(! (useOfAbsClass existsIn graph))
        assert(! (useOfmethAbs existsIn graph))
        assert(! (useOfOtherMethAbs existsIn graph))

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            useOfmeth, mAbs, SupertypeAbstraction, keepOldUse = false).right

        assert(useOfImplClass existsIn g)
        assert(useOfctor existsIn g)

        assert(! (useOfmeth existsIn g))
        assert(! (useOfOtherMeth existsIn g))

        assert(useOfAbsClass existsIn g)
        assert(useOfmethAbs existsIn g)
        assert(useOfOtherMethAbs existsIn g)

      }
    }

    ignore("From method to method delegate"){

    }

    ignore("From field to ??? delegate"){
      //what should we do ?
    }


  }
}
