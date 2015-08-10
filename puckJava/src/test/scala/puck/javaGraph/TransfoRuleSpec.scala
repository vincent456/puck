package puck
package javaGraph


import puck.graph.transformations.rules.Redirection
import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.javaGraph.nodeKind._
import puck.javaGraph.JGraphUtils.{transformationRules => TR}

//import scalaz.syntax.show._
//import puck.util.Debug.showNodeIndex
//println(graph.nodesIndex.shows)




class TransfoRuleSpec extends AcceptanceSpec {

  def getDefinition(g : DependencyGraph, nid : NodeId) : NodeId =
    g.getConcreteNode(nid).definition(g).value

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


    info("abstract class into interface - super type already present")
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



  feature("Redirection"){

    val examplesPath = Settings.testExamplesPath + "/redirection/"
    val typeDeclPath = examplesPath + "typeDecl/"

    info("TypeDecl uses redirection")

    scenario("From class to superType interface"){
      val _ = new ExampleSample(s"$typeDeclPath/ClassToInterfaceSuperType.java"){
        val mUserDecl = fullName2id("p.A.mUser__ClassUsed")
        val theParam = fullName2id("p.A.mUser__ClassUsed.cu")
        val mUserDef = getDefinition(graph, mUserDecl)

        val classUsed = fullName2id("p.ClassUsed")
        val mUsed = fullName2id("p.ClassUsed.mUsed__void")
        val superType = fullName2id("p.SuperType")
        val absmUsed = fullName2id("p.SuperType.mUsed__void")

        val typeUse = Uses(theParam, classUsed)
        assert(typeUse.existsIn(graph))
        assert(Uses(mUserDef, mUsed).existsIn(graph))

        val g2 =
          Redirection.redirectUsesAndPropagate(graph,
            typeUse, AccessAbstraction(superType, SupertypeAbstraction)).right

        assert(Uses(theParam, superType).existsIn(g2))
        assert(Uses(mUserDef, absmUsed).existsIn(g2))

      }
    }

    //val classToClassSupertype

    //val interfaceToInterfaceSuperType

    scenario("From class to delegator class"){
      new ExampleSample(s"$typeDeclPath/ClassToClassDelegate.java"){
        val mUserDecl = fullName2id("p.A.mUser__Delegatee")
        val theParam = fullName2id("p.A.mUser__Delegatee.d")

        val mUserDef = getDefinition(graph, mUserDecl)

        val delegatee = fullName2id("p.Delegatee")
        val mDelegatee = fullName2id("p.Delegatee.mUsed__void")

        val delegator = fullName2id("p.Delegator")
        val mDelegator = fullName2id("p.Delegator.mUsed__void")

        val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
                     .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

        val typeUse = Uses(theParam, delegatee)
        assert(typeUse.existsIn(graph))
        assert(Uses(mUserDef, mDelegatee).existsIn(graph))


        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            typeUse, AccessAbstraction(delegator, DelegationAbstraction) ).right

        assert(Uses(theParam, delegator).existsIn(g2))
        assert(Uses(mUserDef, mDelegator).existsIn(g2))
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

    scenario("From constructor to constructorMethod hosted elsewhere - non static"){
      val _ = new ExampleSample(s"$typeCtorPath/ConstructorToConstructorMethodHostedElsewhere.java"){
        val ctor = fullName2id(s"p.B.B#_void")
        val ctorMethod = fullName2id(s"p.Factory.createB__void")
        val factoryClass = fullName2id(s"p.Factory")
        val factoryCtor = fullName2id(s"p.Factory.Factory#_void")

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)


        val ctorUse = Uses(callerDef, ctor)
        val ctorMethodUse = Uses(callerDef, ctorMethod)

        assert( ctorUse.existsIn(graph) )
        assert( ! ctorMethodUse.existsIn(graph))

        graph.parameters(callerDecl) shouldBe empty

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 = Redirection.redirectUsesAndPropagate(g,
            ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction)).right

        assert( ctorMethodUse.existsIn(g2))
        assert( ! ctorUse.existsIn(g2) )

        val parameters = g2.parameters(callerDecl)
        parameters.size shouldBe 1

        assert(g2.uses(parameters.head, factoryClass))
      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static"){
      val _ = new ExampleSample(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java"){
        val ctor = fullName2id(s"p.B.B#_void")
        val ctorMethod = fullName2id(s"p.B.create__void")
        val constructedClass = fullName2id(s"p.B")

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)

        val userOfTheCallerDecl = fullName2id(s"p.C.mc__void")
        val userOfTheCallerDef = getDefinition(graph, userOfTheCallerDecl)

        val ctorUse = Uses(callerDef, ctor)
        val ctorMethodUse = Uses(callerDef, ctorMethod)

        assert( ctorUse existsIn graph )
        assert( ! (ctorMethodUse existsIn graph))

        graph.parameters(callerDecl) shouldBe empty

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction)).right


        assert( !(ctorUse existsIn g2) )
        assert( ctorMethodUse existsIn g2)

        val parameters = g2.parameters(callerDecl)
        parameters.size shouldBe 1

        assert(g2.uses(parameters.head, constructedClass))

        assert( g2.uses(userOfTheCallerDef, ctor) )
        assert( !g2.uses(userOfTheCallerDef, constructedClass) )

      }
    }

    info("TypeMember uses redirection")

    val typeMemberPath = examplesPath + "typeMember"

    scenario("From method to method superType"){
      val _ = new ExampleSample(s"$typeMemberPath/MethodToMethodSuperType.java") {
        val mUsed = fullName2id("p.Bimpl.m1__void")
        val mAbs = fullName2id("p.B.m1__void")
        val cUsed = fullName2id("p.Bimpl")
        val cUsedCtor = fullName2id("p.Bimpl.Bimpl#_void")

        val otherMused = fullName2id("p.Bimpl.m2__void")
        val otherMabs = fullName2id("p.B.m2__void")
        val cAbs = fullName2id("p.B")

        val userDecl = fullName2id("p.A.m__void")
        val userDef = getDefinition(graph, userDecl)

        val useOfImplClass = Uses(userDef, cUsed)
        val useOfctor = Uses(userDef, cUsedCtor)
        val useOfmeth = Uses(userDef, mUsed)
        val useOfOtherMeth = Uses(userDef, otherMused)

        val useOfAbsClass = Uses(userDef, cAbs)
        val useOfmethAbs = Uses(userDef, mAbs)
        val useOfOtherMethAbs = Uses(userDef, otherMabs)

        assert(useOfImplClass existsIn graph)
        assert(useOfctor existsIn graph)
        assert(useOfmeth existsIn graph)
        assert(useOfOtherMeth existsIn graph)

        assert(! (useOfAbsClass existsIn graph))
        assert(! (useOfmethAbs existsIn graph))
        assert(! (useOfOtherMethAbs existsIn graph))

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            useOfmeth, AccessAbstraction(mAbs, SupertypeAbstraction)).right


        assert(! (useOfImplClass existsIn g))
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
