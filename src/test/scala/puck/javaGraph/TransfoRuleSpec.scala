package puck
package javaGraph

import puck.graph.constraints.{AbstractionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.graph._
import puck.javaGraph.nodeKind._
import puck.util.PuckSystemLogger
import scalaz.{Success, Failure}

import Scenarii._

import scala.language.reflectiveCalls
/**
 * Created by lorilan on 2/25/15.
 */
class TransfoRuleSpec extends AcceptanceSpec {

  /*    def print(name : String)(graph : DependencyGraph): Unit ={
      import puck.graph.io._

      val options =
        PrintingOptions(VisibilitySet.allVisible(graph), printId = true)
        FilesHandler.makeImageFile(None, graph, JavaNode,
        options, puck.testExamplesPath + "/" + name)()
    }*/



  implicit class Contains[G]( t : Iterable[G]){
    def contains(elem : G ) : Boolean = {
      t.exists( _ == elem )
    }
  }

  info("GraphBuilding Tests")// if moved in a specific Spec, errors due to parrallelism and
  // the compilation ensue ... TOFIX ?

  feature("typeUse typeMemberUse relation registration"){
    import typeRelationship._
    scenario("call on field") {
      import callOnField._

      val typeUse = DGEdge.uses(fieldTypeUser, typeUsed)
      val typeMemberUse = DGEdge.uses(methUser, typeMemberUsed)

      /*println("typeMemberUses2typeUsesMap")
      println(graph.typeMemberUses2typeUsesMap.content.mkString("\n"))
      println("typeUses2typeMemberUsesMap")
      println(graph.typeUses2typeMemberUsesMap.content.mkString("\n"))

      quickFrame(graph)*/
      assert( graph.typeMemberUsesOf(typeUse) contains typeMemberUse )
    }

    scenario("call on method's parameter"){
      import callOnParameter._

      val typeUse = DGEdge.uses(mUser, classUsed)
      val typeMemberUse = DGEdge.uses(mUser, mUsed)

      assert( graph.typeMemberUsesOf(typeUse) contains typeMemberUse )

    }
    scenario("call from local variable"){
      import callOnLocalVariable._
      val typeUse = DGEdge.uses(mUser, classUsed)
      val typeMemberUse = DGEdge.uses(mUser, mUsed)

      assert( graph.typeMemberUsesOf(typeUse) contains typeMemberUse )
    }
    scenario("chained call"){
      import chainedCall._

      val typeUse = DGEdge.uses(mIntermediate, classUsed)
      val typeMemberUse = DGEdge.uses(mUser, mUsed)

      assert( graph.typeMemberUsesOf(typeUse) contains typeMemberUse )
    }
  }

  feature("Abstraction registration"){
    import abstractionRegistration._
    scenario("one class one interface"){
      import interfaceSupertype._

      assert( graph.abstractions(classUsed) contains ((superType, SupertypeAbstraction)) )
      assert( graph.abstractions(mUsed) contains ((absmUsed, SupertypeAbstraction)) )
    }
  }



  info("Transformation Rules Tests")

  def assertSuccess[G](t : Try[G])(f : G => Unit) : Unit = {
    t match {
      case Failure(_) => assert(false)
      case Success(g) => f(g)
    }
  }
  implicit class TestOption[G]( t : Option[G]){
    def assertSome(f : G => Unit) : Unit = {
      t match {
        case None => assert(false)
        case Some(g) => f(g)
      }
    }
  }
  
  feature("Move"){
    val case1 = methodUsesViaThisField
    scenario("Move class"){
      val (g, package1) = introPackage(case1.graph, "p1", case1.rootPackage)
      assertSuccess(TR.moveTo(g, case1.classBNode.id, package1.id)) {
        g2 => g2.container(case1.classBNode.id).assertSome {
          pid0 => assert(pid0 == package1.id)
        }
      }
    }

    scenario("Move method used by this"){
      import usesThisMethod.{graph => initialGraph, _}

      val (classB, g2) = initialGraph.addConcreteNode("B", Class, None)
      g2.addContains(rootPackage, classB.id)

      assertSuccess(TR.moveTo(g2, methMa2, classB.id)){
        g3 =>
          g3.container(methMa2).assertSome { cid =>
            assert(cid == classB.id)
          }
          assert(g3.uses(methMa1, methMa2))
      }
    }
  }

  feature("Redirection"){

    val examplesPath = puck.testExamplesPath + "/redirection/"
    val typeDeclPath = examplesPath + "typeDecl/"

    info("TypeDecl uses redirection")

    def classToAbsScenario
    ( graph : DependencyGraph,
      //typeUser : NodeId,
      typeMemberUser : NodeId,
      typeUsed : NodeId,
      typeMemberUsed : NodeId,
      newTypeUsed : NodeId,
      newTypeMemberUsed : NodeId,
      policy : AbstractionPolicy
      ) : Unit = {
      val typeUse = DGEdge.uses(typeMemberUser, typeUsed)
      assert(typeUse.existsIn(graph))
      assert(DGEdge.uses(typeMemberUser, typeMemberUsed).existsIn(graph))
      assertSuccess(TR.redirectUsesAndPropagate(graph, typeUse, newTypeUsed, policy)) {
        case g2 =>
          assert(DGEdge.uses(typeMemberUser, newTypeUsed).existsIn(g2))
          assert(DGEdge.uses(typeMemberUser, newTypeMemberUsed).existsIn(g2))
      }
    }

    scenario("From class to superType interface"){
      val _ = new ExampleSample(typeDeclPath + "classToInterfaceSuperType/A.java"){
        val rootPackage = fullName2id("classToInterfaceSuperType")
        val mUser = fullName2id("classToInterfaceSuperType.A.mUser__ClassUsed")

        val classUsed = fullName2id("classToInterfaceSuperType.ClassUsed")
        val mUsed = fullName2id("classToInterfaceSuperType.ClassUsed.mUsed__void")
        val superType = fullName2id("classToInterfaceSuperType.SuperType")
        val absmUsed = fullName2id("classToInterfaceSuperType.SuperType.mUsed__void")

        classToAbsScenario(graph, mUser,
          classUsed, mUsed,
          superType, absmUsed,
          DelegationAbstraction)
      }
    }

    //val classToClassSupertype

    //val interfaceToInterfaceSuperType

    ignore("From class to delegator class"){
      new ExampleSample(typeDeclPath + "classToClassDelegate/A.java"){
        val p = "classToClassDelegate"
        val mUser = fullName2id(s"$p.A.mUser__Delegatee")
        val delegatee = fullName2id(s"$p.Delegatee")
        val mDelegatee = fullName2id(s"$p.Delegatee.mUsed__void")

        val delegator = fullName2id(s"$p.Delegator")
        val mDelegator = fullName2id(s"$p.Delegator.mUsed__void")

        val g = graph.addAbstraction(delegatee, (delegator, DelegationAbstraction))
                     .addAbstraction(mDelegatee, (mDelegator, DelegationAbstraction))

        classToAbsScenario(graph, mUser,
          delegatee, mDelegatee,
          delegator, mDelegator,
          DelegationAbstraction)
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
