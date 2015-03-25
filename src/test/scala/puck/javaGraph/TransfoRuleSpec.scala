package puck
package javaGraph

import org.scalatest.OptionValues
import puck.graph.constraints.{AbstractionPolicy, SupertypeAbstraction, DelegationAbstraction}
import puck.graph._
import puck.util.PuckSystemLogger
import scalaz.{Success, Failure}

import Scenarii._

import scala.language.reflectiveCalls
/**
 * Created by lorilan on 2/25/15.
 */
class TransfoRuleSpec extends AcceptanceSpec with OptionValues {

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

  feature("Move"){
    val examplesPath = puck.testExamplesPath + "/move"

    scenario("Move top level class"){
      val p = "topLevelClass"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java",
        s"$examplesPath/topLevelClass/Empty.java"){
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

        assertSuccess(TR.moveTypeMember(graph.withLogger(new PuckSystemLogger(_ => true)), methMa, classB)){
          g2 =>
            quickFrame(g2)
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

        assertSuccess(TR.moveTypeMember(graph, methMa2, classB)){
          g2 =>
            assert(g2.container(methMa2).value == classB)
            assert(g2.uses(methMa1, methMa2))
            assert(g2.uses(methMa2, classA))
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
