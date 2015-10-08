package puck.javaGraph.commutativity

import puck.javaGraph.nodeKind.Field
import puck.{GetDefinitionValue, AcceptanceSpec, QuickFrame, Settings}
import puck.graph.comparison.Mapping
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.{AccessAbstraction, Uses}
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Redirection}
import puck.javaGraph.ScenarioFactory
import puck.Settings.outDir

class CommutativityRedirect
  extends AcceptanceSpec
  with GetDefinitionValue{

  val examplesPath = Settings.testExamplesPath + "/redirection/"

  feature("TypeDecl uses redirection") {

    val typeDeclPath = examplesPath + "typeDecl/"

    scenario("From class to superType interface") {
      val _ = new ScenarioFactory(s"$typeDeclPath/ClassToInterfaceSuperType.java") {
        val theParam = fullName2id("p.A.mUser__ClassUsed.cu")

        val classUsed = fullName2id("p.ClassUsed")
        val superType = fullName2id("p.SuperType")

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            Uses(theParam, classUsed),
            AccessAbstraction(superType, SupertypeAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert(Mapping.equals(g, recompiledEx.graph))

      }
    }

    //val classToClassSupertype

    //val interfaceToInterfaceSuperType

    ignore("From class to delegator class") {
      new ScenarioFactory(s"$typeDeclPath/ClassToClassDelegate.java") {
        val theParam = fullName2id("p.A.mUser__Delegatee.d")

        val delegatee = fullName2id("p.Delegatee")
        val mDelegatee = fullName2id("p.Delegatee.mUsed__void")

        val delegator = fullName2id("p.Delegator")
        val mDelegator = fullName2id("p.Delegator.mUsed__void")

        QuickFrame(graph, "g")

        val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
          .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            Uses(theParam, delegatee),
            AccessAbstraction(delegator, DelegationAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
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
        val ctor = fullName2id(s"p.B.B#_void")
        val ctorMethod = fullName2id(s"p.Factory.createB__void")

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    scenario("From constructor to constructorMethod hosted elsewhere - non static, field") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedElsewhere.java") {
        val ctor = fullName2id(s"p.B.B#_void")
        val ctorMethod = fullName2id(s"p.Factory.createB__void")

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right


        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

//        QuickFrame(g2, "g2")
//        QuickFrame(recompiledEx.graph, "recompiledEx.graph")
        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static, parameter") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java") {
        val ctor = fullName2id(s"p.B.B#_void")
        val ctorMethod = fullName2id(s"p.B.create__void")

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert(Mapping.equals(g2, recompiledEx.graph))

      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static, field") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java") {
        val ctor = fullName2id(s"p.B.B#_void")
        val ctorMethod = fullName2id(s"p.B.create__void")

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert(Mapping.equals(g2, recompiledEx.graph))

      }
    }
  }
  feature("TypeMember uses redirection"){

        val typeMemberPath = examplesPath + "typeMember"

        scenario("From method to method superType"){
          val _ = new ScenarioFactory(s"$typeMemberPath/MethodToMethodSuperType.java") {
            val mUsed = fullName2id("p.Bimpl.m1__void")
            val mAbs = fullName2id("p.B.m1__void")

            val userDecl = fullName2id("p.A.m__void")
            val userDef = getDefinition(graph, userDecl)


            val g =
              Redirection.redirectUsesAndPropagate(graph,
                Uses(userDef, mUsed), AccessAbstraction(mAbs, SupertypeAbstraction)).right


            val recompiledEx = applyChangeAndMakeExample(g, outDir)
            assert( Mapping.equals(g, recompiledEx.graph) )


          }
        }

        ignore("From method to method delegate"){

        }

        ignore("From field to ??? delegate"){
          //what should we do ?
        }


  }
}
