package puck
package javaGraph

import org.scalatest.{Matchers, OptionValues}
import puck.graph.constraints.{SupertypeAbstraction, DelegationAbstraction}
import puck.graph._
import puck.graph.transformations.CreateParameter
import puck.javaGraph.nodeKind.Interface
import puck.util.PuckSystemLogger
import scalaz.{Success, Failure}

import Scenarii._

import scala.language.reflectiveCalls
/**
 * Created by lorilan on 2/25/15.
 */
class TransfoRuleSpec extends AcceptanceSpec with OptionValues{

  def assertSuccess[G](t : Try[G])(f : G => Unit) : Unit = {
    t match {
      case Failure(_) => assert(false)
      case Success(g) => f(g)
    }
  }

  feature("Intro"){
    val examplesPath = puck.testExamplesPath + "/intro"
    scenario("Intro interface - no existing super type"){
      val p = "introInterfaceNoExistingSuperType"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java") {
        val classA = fullName2id(s"$p.A")
        val methM = fullName2id(s"$p.A.m__void")
        val field = fullName2id(s"$p.A.f")

        assert( graph.directSuperTypes(classA).isEmpty )

        assert( graph.abstractions(classA).isEmpty )
        assert( graph.abstractions(methM).isEmpty )
        assert( graph.abstractions(field).isEmpty )


        assertSuccess(TR.createAbstraction(graph, graph.getConcreteNode(classA),
          Interface, SupertypeAbstraction)){
          case (itc, g) =>
            assert( g.isa(classA, itc.id) )

            g.abstractions(classA).size shouldBe 1
            g.abstractions(methM).size shouldBe 1
            assert( g.abstractions(field).isEmpty ,
              "Field cannot be exposed in an interface")

        }
      }

    }

    scenario("Intro interface - no existing super type - method self use in class"){
      val p = "introInterfaceNoExistingSuperTypeWithSelfUse"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java") {
        val classA = fullName2id(s"$p.A")
        val methM = fullName2id(s"$p.A.m__void")

        val methMUser = fullName2id(s"$p.A.methodUser__A")

        assert( graph.directSuperTypes(classA).isEmpty )

        assert( graph.uses(methMUser, classA) )
        assert( graph.uses(methMUser, methM) )

        assert( graph.abstractions(classA).isEmpty )
        assert( graph.abstractions(methM).isEmpty )
        assert( graph.abstractions(methMUser).isEmpty )


        assertSuccess(TR.createAbstraction(graph, graph.getConcreteNode(classA),
          Interface, SupertypeAbstraction)){
          case (itc, g) =>
            assert( g.isa(classA, itc.id) )

            g.abstractions(classA).size shouldBe 1
            g.abstractions(methM).size shouldBe 1
            g.abstractions(methMUser).size shouldBe 1


            val methMAbs = g.abstractions(methM).head._1

            assert( g.uses(methMUser, methMAbs) )
            assert( g.uses(methMUser, itc.id) )
        }
      }

    }

    scenario("Intro interface - no existing super type - field self use in class"){
      val p = "introInterfaceNoExistingSuperTypeWithSelfUse"
      val _ = new ExampleSample(s"$examplesPath/$p/B.java") {
        val classB = fullName2id(s"$p.B")
        val field = fullName2id(s"$p.B.f")

        val fieldUserThatShouldNotBeInInterface =
          fullName2id(s"$p.B.fieldUserThatShouldNotBeInInterface__B")

        assert( graph.directSuperTypes(classB).isEmpty )

        assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
        assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )

        assert( graph.abstractions(classB).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty )
        quickFrame(graph)
        assertSuccess(TR.createAbstraction(graph.withLogger(new PuckSystemLogger(_ => true)), graph.getConcreteNode(classB),
          Interface, SupertypeAbstraction)){
          case (itc, g) =>
            assert( g.isa(classB, itc.id) )

            g.abstractions(classB).size shouldBe 1
            assert( g.abstractions(field).isEmpty ,
              "Field cannot be exposed in an interface")
            assert( g.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty,
              "Method use concrete class field, should not be abstracted")

            assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
            assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )

        }
      }
    }

    scenario("Intro interface - no existing super type - field use of self type parameter in class"){
      val p = "introInterfaceNoExistingSuperTypeWithSelfUse"
      val _ = new ExampleSample(s"$examplesPath/$p/C.java") {
        val classA = fullName2id(s"$p.C")
        val field = fullName2id(s"$p.C.f")

        val fieldUserThatCanBeInInterface =
          fullName2id(s"$p.C.fieldUserThatCanBeInInterface__void")

        assert( graph.directSuperTypes(classA).isEmpty )

        //assert( !graph.uses(fieldUserThatCanBeInInterface, classA) )
        assert( graph.uses(fieldUserThatCanBeInInterface, field) )


        assert( graph.abstractions(classA).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatCanBeInInterface).isEmpty )

        assertSuccess(TR.createAbstraction(graph, graph.getConcreteNode(classA),
          Interface, SupertypeAbstraction)){
          case (itc, g) =>
            assert( g.isa(classA, itc.id) )

            g.abstractions(classA).size shouldBe 1
            assert( g.abstractions(field).isEmpty ,
              "Field cannot be exposed in an interface")
            g.abstractions(fieldUserThatCanBeInInterface).size shouldBe 1

            //assert( graph.uses(fieldUserThatCanBeInInterface, classA) )
            assert( graph.uses(fieldUserThatCanBeInInterface, field) )
        }
      }
    }

    ignore("Intro interface - cyclic uses in class (recursion)"){}
    ignore("Intro interface - super interface existing"){}
    ignore("Intro interface - super class existing"){}

  }

  feature("Move"){
    val examplesPath = puck.testExamplesPath + "/move"

    scenario("Move top level class"){
      val p = "topLevelClass"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java",
        s"$examplesPath/$p/Empty.java"){
        val package1 = fullName2id(s"$p.p1")
        val package2 = fullName2id(s"$p.p2")
        val classA = fullName2id(s"$p.p1.A")
        val methA = fullName2id(s"$p.p1.A.ma__void")

        val classB = fullName2id(s"$p.p1.B")
        val methB = fullName2id(s"$p.p1.B.mb__void")

        assert(graph.container(classA).value == package1)
        assert(graph.uses(methB, classA))
        assert(graph.uses(methB, methA))

        assertSuccess(TR.moveTypeDecl(graph, classA, package2)){
          g2 =>
            assert(g2.container(classA).value == package2)
            assert(graph.uses(methB, classA))
            assert(graph.uses(methB, methA))
        }
      }
    }

    scenario("Move method not used by this"){
      val p = "methodNotUsedByThis"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val rootPackage = fullName2id(s"$p")

        val classA = fullName2id(s"$p.A")
        val methMa = fullName2id(s"$p.A.ma__void")
        val methUser = fullName2id(s"$p.C.mc__void")

        val classB = fullName2id(s"$p.B")

        assert(graph.container(methMa).value == classA)
        assert(graph.uses(methUser, methMa))

        assertSuccess(TR.moveTypeMember(graph,
          methMa, classB)){
          g2 =>
            assert(g2.container(methMa).value == classB)
            assert(g2.uses(methUser, methMa))
        }
      }
    }

    scenario("Move method used by this"){
      val p = "methodUsedByThis"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val rootPackage = fullName2id(s"$p")

        val classA = fullName2id(s"$p.A")
        val methMa1 = fullName2id(s"$p.A.ma1__void")
        val methMa2 = fullName2id(s"$p.A.ma2__void")

        val classB = fullName2id(s"$p.B")

        assert(graph.container(methMa2).value == classA)
        assert(graph.uses(methMa1, methMa2))
        assert(! graph.uses(methMa1, classB))

        assertSuccess(TR.moveTypeMember(graph, methMa2, classB, CreateParameter)){
          g2 =>
            assert(g2.container(methMa2).value == classB)
            assert(g2.uses(methMa1, methMa2))
            assert(g2.uses(methMa1, classB))
        }
      }
    }


    ignore("Move method not used by this to class of a parameter"){
      val p = "methodNotUsedByThisToParameterClass"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val rootPackage = fullName2id(s"$p")

        val classA = fullName2id(s"$p.A")
        val methMa = fullName2id(s"$p.A.ma__B")
        val methUser = fullName2id(s"$p.C.mc__void")

        val classB = fullName2id(s"$p.B")

        val methMaNode = graph.getConcreteNode(methMa)
        methMaNode.styp.value match {
          case MethodType(in, _) =>
            assert(1 == in.length && in.ids.contains(classB))
        }

        assert(graph.container(methMa).value == classA)
        assert(graph.uses(methUser, methMa))

        assertSuccess(TR.moveTypeMember(graph, methMa, classB)){
          g2 =>

            assert(g2.container(methMa).value == classB)
            assert(g2.uses(methUser, methMa))

            val methMaNode = g2.getConcreteNode(methMa)
            methMaNode.styp.value match {
              case MethodType(in, _) => assert(0 == in.length)
            }
        }
      }
    }


  }

  feature("Redirection"){

    val examplesPath = puck.testExamplesPath + "/redirection/"
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

        val typeUse = DGEdge.uses(mUser, classUsed)
        assert(typeUse.existsIn(graph))
        assert(DGEdge.uses(mUser, mUsed).existsIn(graph))
        assertSuccess(TR.redirectUsesAndPropagate(graph, typeUse, superType,
          SupertypeAbstraction)) {
          case g2 =>
            assert(DGEdge.uses(mUser, superType).existsIn(g2))
            assert(DGEdge.uses(mUser, absmUsed).existsIn(g2))
        }
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

        val g = graph.addAbstraction(delegatee, (delegator, DelegationAbstraction))
                     .addAbstraction(mDelegatee, (mDelegator, DelegationAbstraction))

        val typeUse = DGEdge.uses(mUser, delegatee)
        assert(typeUse.existsIn(graph))
        assert(DGEdge.uses(mUser, mDelegatee).existsIn(graph))
        assertSuccess(TR.redirectUsesAndPropagate(graph, typeUse, delegator, DelegationAbstraction)) {
          case g2 =>
            assert(DGEdge.uses(mUser, delegator).existsIn(g2))
            assert(DGEdge.uses(mUser, mDelegator).existsIn(g2))
        }
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

          val ctorUse = DGEdge.uses(caller, ctor)
          assert( ctorUse.existsIn(graph) )

          val ctorMethodUse =DGEdge.uses(caller, ctorMethod)
          assert( ! ctorMethodUse.existsIn(graph))

          val g = graph.addAbstraction(ctor, (ctorMethod, DelegationAbstraction))
          assertSuccess(TR.redirectUsesAndPropagate(g, ctorUse, ctorMethod, DelegationAbstraction)){
            g2 =>
              assert( ctorMethodUse.existsIn(g2))
              assert( ! ctorUse.existsIn(g2) )
              assert(g2.uses(caller, factoryClass))
              //??
              assert(g2.uses(caller, factoryCtor))
          }
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

        val constructedClassUse = DGEdge.uses(caller, constructedClass)
        val ctorUse = DGEdge.uses(caller, ctor)
        assert( ctorUse existsIn graph )

        val ctorMethodUse = DGEdge.uses(caller, ctorMethod)
        assert( ! (ctorMethodUse existsIn graph))

        val g = graph.addAbstraction(ctor, (ctorMethod, DelegationAbstraction))
        assertSuccess(TR.redirectUsesAndPropagate(g, ctorUse, ctorMethod, DelegationAbstraction)){
          g2 =>
            assert( ctorMethodUse existsIn g2)
            assert( !(ctorUse existsIn g2) )
            assert( constructedClassUse existsIn g2)
            assert( g2.uses(userOfTheCaller, ctor) )
            assert( g2.uses(userOfTheCaller, constructedClass) )

        }
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

        val useOfImplClass = DGEdge.uses(user, cUsed)
        val useOfctor = DGEdge.uses(user, cUsedCtor)
        val useOfmeth = DGEdge.uses(user, mUsed)
        val useOfOtherMeth = DGEdge.uses(user, otherMused)

        val useOfAbsClass = DGEdge.uses(user, cAbs)
        val useOfmethAbs = DGEdge.uses(user, mAbs)
        val useOfOtherMethAbs = DGEdge.uses(user, otherMabs)

        assert(useOfImplClass existsIn graph)
        assert(useOfctor existsIn graph)
        assert(useOfmeth existsIn graph)
        assert(useOfOtherMeth existsIn graph)

        assert(! (useOfAbsClass existsIn graph))
        assert(! (useOfmethAbs existsIn graph))
        assert(! (useOfOtherMethAbs existsIn graph))

        assertSuccess(TR.redirectUsesAndPropagate(graph, useOfmeth, mAbs, SupertypeAbstraction)){
          g =>
            assert(useOfImplClass existsIn g)
            assert(useOfctor existsIn g)

            assert(! (useOfmeth existsIn g))
            assert(! (useOfOtherMeth existsIn g))

            assert(useOfAbsClass existsIn g)
            assert(useOfmethAbs existsIn g)
            assert(useOfOtherMethAbs existsIn g)
        }
      }
    }

    ignore("From method to method delegate"){

    }

    ignore("From field to ??? delegate"){
      //what should we do ?
    }


  }
}
