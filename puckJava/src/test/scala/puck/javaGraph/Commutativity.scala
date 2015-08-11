package puck.javaGraph

import java.io.File

import puck.graph.transformations.rules.Redirection
import puck.graph._
import puck.graph.comparison.Mapping
import puck.util.Debug
import puck.{QuickFrame, Settings, AcceptanceSpec}
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.javaGraph.nodeKind.Interface
import puck.javaGraph.JGraphUtils.{transformationRules => Rules}

class Commutativity extends AcceptanceSpec {

  def getDefinition(g : DependencyGraph, nid : NodeId) : NodeId =
    g.getConcreteNode(nid).definition(g).value

  val outDir = new File(Settings.tmpDir + "testPuck")

  feature("Commutativity - abstract") {
    val examplesPath = Settings.testExamplesPath + "/intro"

    val noSuperTypePath = examplesPath + "/interface/noExistingSuperType/"

    scenario("extract interface") {

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

//        QuickFrame(g, "g")
//        QuickFrame(recompiledEx.graph, "recompiled")
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }
  }

  feature("Redirection"){

    val examplesPath = Settings.testExamplesPath + "/redirection/"
    val typeDeclPath = examplesPath + "typeDecl/"

    info("TypeDecl uses redirection")

    scenario("From class to superType interface"){
      val _ = new ExampleSample(s"$typeDeclPath/ClassToInterfaceSuperType.java"){
        val theParam = fullName2id("p.A.mUser__ClassUsed.cu")

        val classUsed = fullName2id("p.ClassUsed")
        val superType = fullName2id("p.SuperType")

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            Uses(theParam, classUsed),
            AccessAbstraction(superType, SupertypeAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    //val classToClassSupertype

    //val interfaceToInterfaceSuperType

    ignore("From class to delegator class"){
      new ExampleSample(s"$typeDeclPath/ClassToClassDelegate.java"){
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
            AccessAbstraction(delegator, DelegationAbstraction) ).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert( Mapping.equals(g2, recompiledEx.graph) )
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

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 = Redirection.redirectUsesAndPropagate(g,
          Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert( Mapping.equals(g2, recompiledEx.graph) )
      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static"){
      val _ = new ExampleSample(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java"){
        val ctor = fullName2id(s"p.B.B#_void")
        val ctorMethod = fullName2id(s"p.B.create__void")

        val callerDecl = fullName2id(s"p.A.m__void")
        val callerDef = getDefinition(graph, callerDecl)

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert( Mapping.equals(g2, recompiledEx.graph) )

      }
    }

//    info("TypeMember uses redirection")
//
//    val typeMemberPath = examplesPath + "typeMember"
//
//    scenario("From method to method superType"){
//      val _ = new ExampleSample(s"$typeMemberPath/MethodToMethodSuperType.java") {
//        val mUsed = fullName2id("p.Bimpl.m1__void")
//        val mAbs = fullName2id("p.B.m1__void")
//
//        val userDecl = fullName2id("p.A.m__void")
//        val userDef = getDefinition(graph, userDecl)
//
//
//        val g =
//          Redirection.redirectUsesAndPropagate(graph,
//            Uses(userDef, mUsed), AccessAbstraction(mAbs, SupertypeAbstraction)).right
//
//
//        val recompiledEx = applyChangeAndMakeExample(g, outDir)
//        assert( Mapping.equals(g, recompiledEx.graph) )
//
//
//      }
//    }
//
//    ignore("From method to method delegate"){
//
//    }
//
//    ignore("From field to ??? delegate"){
//      //what should we do ?
//    }


  }

}

