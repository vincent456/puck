package puck.javaGraph.transfoRules

import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Redirection}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind._
import puck.{AcceptanceSpec, Settings}

//import scalaz.syntax.show._
//import puck.util.Debug.showNodeIndex
//println(graph.nodesIndex.shows)




class RedirectSpec
  extends AcceptanceSpec {

  val examplesPath = Settings.testExamplesPath + "/redirection/"


  feature("TypeDecl uses redirection") {

    val typeDeclPath = examplesPath + "typeDecl/"

    scenario("From class to superType interface") {
      val _ = new ScenarioFactory(s"$typeDeclPath/ClassToInterfaceSuperType.java") {
        val mUserDecl = fullName2id("p.A.mUser(ClassUsed)")
        val theParam = fullName2id("p.A.mUser(ClassUsed).cu")
        val mUserDef = fullName2id("p.A.mUser(ClassUsed).Definition")

        val classUsed = fullName2id("p.ClassUsed")
        val mUsed = fullName2id("p.ClassUsed.mUsed()")
        val superType = fullName2id("p.SuperType")
        val absmUsed = fullName2id("p.SuperType.mUsed()")

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

    scenario("From class to delegator class") {
      new ScenarioFactory(s"$typeDeclPath/ClassToClassDelegate.java") {
        val mUserDecl = fullName2id("p.A.mUser(Delegatee)")
        val theParam = fullName2id("p.A.mUser(Delegatee).d")

        val mUserDef = fullName2id("p.A.mUser(Delegatee).Definition")

        val delegatee = fullName2id("p.Delegatee")
        val mDelegatee = fullName2id("p.Delegatee.mUsed()")

        val delegator = fullName2id("p.Delegator")
        val mDelegator = fullName2id("p.Delegator.mUsed()")

        val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
          .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

        val typeUse = Uses(theParam, delegatee)
        assert(typeUse.existsIn(graph))
        assert(Uses(mUserDef, mDelegatee).existsIn(graph))


        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            typeUse, AccessAbstraction(delegator, DelegationAbstraction)).right

        assert(Uses(theParam, delegator).existsIn(g2))
        assert(Uses(mUserDef, mDelegator).existsIn(g2))
      };
      ()

    }

    /*val interfaceToClassDelegate = new ExampleSample(typeDeclPath + "interfaceToClassDelegate/A.java"){
      val rootPackage = fullName2id("interfaceToClassDelegate")
      val mUser = fullName2id("interfaceToClassDelegate.A.mUser__I")
      val interface = fullName2id("interfaceToClassDelegate.I")
      val delegator = fullName2id("interfaceToClassDelegate.Delegator")
    }*/

  }
  feature("TypeConstructor uses redirection") {

    val typeCtorPath = examplesPath + "typeConstructor"

    scenario("From constructor to constructorMethod hosted elsewhere - non static, parameter") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedElsewhere.java") {
        val ctor = fullName2id(s"p.B.B()")
        val ctorMethod = fullName2id(s"p.Factory.createB()")
        val factoryClass = fullName2id(s"p.Factory")
        val factoryCtor = fullName2id(s"p.Factory.Factory()")

        val callerDecl = fullName2id(s"p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")


        val ctorUse = Uses(callerDef, ctor)
        val ctorMethodUse = Uses(callerDef, ctorMethod)

        assert(ctorUse.existsIn(graph))
        assert(!ctorMethodUse.existsIn(graph))

        graph.parametersOf(callerDecl) shouldBe empty

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right

        assert(ctorMethodUse.existsIn(g2))
        assert(!ctorUse.existsIn(g2))

        val parameters = g2.parametersOf(callerDecl)
        parameters.size shouldBe 1

        assert(g2.uses(parameters.head, factoryClass))
      }
    }

    scenario("From constructor to constructorMethod hosted elsewhere - non static, field") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedElsewhere.java") {
        val ctor = fullName2id(s"p.B.B()")
        val ctorMethod = fullName2id(s"p.Factory.createB()")
        val factoryClass = fullName2id(s"p.Factory")
        val factoryCtor = fullName2id(s"p.Factory.Factory()")

        val callerDef = fullName2id("p.A.m().Definition")

        val callerHostClass = fullName2id(s"p.A")


        val ctorUse = Uses(callerDef, ctor)
        val ctorMethodUse = Uses(callerDef, ctorMethod)

        assert(ctorUse.existsIn(graph))
        assert(!ctorMethodUse.existsIn(graph))

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right

        assert(ctorMethodUse.existsIn(g2))
        assert(!ctorUse.existsIn(g2))

        g2.content(callerHostClass).size shouldBe (graph.content(callerHostClass).size + 1)

        //assert(g2.uses(parameters.head, factoryClass))
      }
    }


    scenario("From constructor to constructorMethod hosted by self - non static, parameter") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java") {
        val ctor = fullName2id(s"p.B.B()")
        val ctorMethod = fullName2id(s"p.B.create()")
        val constructedClass = fullName2id(s"p.B")

        val callerDecl = fullName2id(s"p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val userOfTheCallerDecl = fullName2id(s"p.C.mc()")
        val userOfTheCallerDef = fullName2id("p.C.mc().Definition")

        val ctorUse = Uses(callerDef, ctor)
        val ctorMethodUse = Uses(callerDef, ctorMethod)

        assert(ctorUse existsIn graph)
        assert(!(ctorMethodUse existsIn graph))

        graph.parametersOf(callerDecl) shouldBe empty

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right


        assert(!(ctorUse existsIn g2))
        assert(ctorMethodUse existsIn g2)

        val parameters = g2.parametersOf(callerDecl)
        parameters.size shouldBe 1

        assert(g2.uses(parameters.head, constructedClass))

        assert(g2.uses(userOfTheCallerDef, ctor))
        assert(!g2.uses(userOfTheCallerDef, constructedClass))

      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static, field") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java") {
        val ctor = fullName2id(s"p.B.B()")
        val ctorMethod = fullName2id(s"p.B.create()")
        val constructedClass = fullName2id(s"p.B")

        val callerDecl = fullName2id(s"p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val userOfTheCallerDecl = fullName2id(s"p.C.mc()")
        val userOfTheCallerDef = fullName2id("p.C.mc().Definition")

        val ctorUse = Uses(callerDef, ctor)
        val ctorMethodUse = Uses(callerDef, ctorMethod)

        val callerHostClass = fullName2id(s"p.A")


        assert(ctorUse existsIn graph)
        assert(!(ctorMethodUse existsIn graph))


        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right


        assert(!(ctorUse existsIn g2))
        assert(ctorMethodUse existsIn g2)

        g2.content(callerHostClass).size shouldBe (graph.content(callerHostClass).size + 1)

        val delegate = g2.nodes.find(n => n.name(g2).endsWith("_delegate")).value

        assert(g2.uses(g2.definitionOf_!(delegate.id), ctor))
        assert(!g2.uses(userOfTheCallerDef, constructedClass))

      }
    }
  }
  feature("TypeMember uses redirection"){

    val typeMemberPath = examplesPath + "typeMember"

    scenario("From method to method superType"){
      val _ = new ScenarioFactory(s"$typeMemberPath/MethodToMethodSuperType.java") {
        val mUsed = fullName2id("p.Bimpl.m1()")
        val mAbs = fullName2id("p.B.m1()")
        val cUsed = fullName2id("p.Bimpl")
        val cUsedCtor = fullName2id("p.Bimpl.Bimpl()")

        val otherMused = fullName2id("p.Bimpl.m2()")
        val otherMabs = fullName2id("p.B.m2()")
        val cAbs = fullName2id("p.B")

        val userDecl = fullName2id("p.A.m()")
        val userDef = fullName2id("p.A.m().Definition")

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
