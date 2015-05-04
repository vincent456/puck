package puck
package javaGraph

//import nodeKind._
import puck.graph.{DependencyGraph, DGEdge}
import puck.graph.constraints.SupertypeAbstraction

class GraphBuildingSpec extends AcceptanceSpec {

  val graphBuildingExamplesPath = Settings.testExamplesPath + "/graphBuilding/"

  feature("use registration"){
    val examplesPath = graphBuildingExamplesPath +  "useRegistration/"

    //3 cas de typeUse vers thisClass
    // sig uses : param type
    // body uses : local var or static access
    // thisClass uses : call a sibling method or field
    scenario("this use explicit") {
      val p = "thisUseExplicit"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java") {
        val clazz = fullName2id(s"$p.A")
        val methM = fullName2id(s"$p.A.m__void")
        val mUserViaThis = fullName2id(s"$p.A.mUserViaThis__void")
        val mUserViaParameter = fullName2id(s"$p.A.mUserViaParameter__A")

        //methodUse
        assert( graph.uses(mUserViaThis, methM) )
        //typeUse
        assert( graph.uses(clazz, clazz) )


        assert( graph.uses(mUserViaParameter, methM) )
        assert( graph.uses(mUserViaParameter, clazz) )

      }
    }

  }

  feature("typeUse typeMemberUse relation registration"){
    val examplesPath = graphBuildingExamplesPath +  "typeRelationship/"
    scenario("call on field") {
      val p = "callOnField"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val fieldTypeUser = fullName2id(s"$p.A.b")
        val methUser = fullName2id(s"$p.A.ma__void")

        val typeUsed = fullName2id(s"$p.B")
        val typeMemberUsed = fullName2id(s"$p.B.mb__void")

        val typeUse = DGEdge.uses(fieldTypeUser, typeUsed)
        val typeMemberUse = (methUser, typeMemberUsed)

        /*println("typeMemberUses2typeUsesMap")
        println(graph.typeMemberUses2typeUsesMap.content.mkString("\n"))
        println("typeUses2typeMemberUsesMap")
        println(graph.typeUses2typeMemberUsesMap.content.mkString("\n"))

        quickFrame(graph)*/
        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)

      }


    }

    scenario("call on method's parameter"){
      val p = "callOnParameter"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val mUser = fullName2id(s"$p.A.ma__B")
        val classUsed = fullName2id(s"$p.B")
        val mUsed = fullName2id(s"$p.B.mb__void")

        val typeUse = DGEdge.uses(mUser, classUsed)
        val typeMemberUse = (mUser, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)

      }
    }

    scenario("call from local variable"){
      val p = "callOnLocalVariable"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val mUser = fullName2id(s"$p.A.ma__void")
        val mUsed = fullName2id(s"$p.B.mb__void")

        val classUsed = fullName2id(s"$p.B")

        val typeUse = DGEdge.uses(mUser, classUsed)
        val typeMemberUse = (mUser, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      }
    }

    scenario("chained call"){
      val p = "chainedCall"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){
        val mUser = fullName2id(s"$p.A.ma__void")
        val mUsed = fullName2id(s"$p.C.mc__void")
        val mIntermediate = fullName2id(s"$p.B.mb__void")
        val classUsed = fullName2id(s"$p.C")

        val typeUse = DGEdge.uses(mIntermediate, classUsed)
        val typeMemberUse = (mUser, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      }
    }
  }

  feature("Abstraction registration"){
    val examplesPath = graphBuildingExamplesPath +  "abstractionRegistration/"
    scenario("one class one interface"){
      val p = "interfaceSupertype"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val classUsed = fullName2id(s"$p.A")
        val mUsed = fullName2id(s"$p.A.ma__void")
        val superType = fullName2id(s"$p.SuperType")
        val absmUsed = fullName2id(s"$p.SuperType.ma__void")

        graph.abstractions(classUsed) should contain ( (superType, SupertypeAbstraction) )
        graph.abstractions(mUsed) should contain ( (absmUsed, SupertypeAbstraction) )
      }
    }
  }


  feature("Isa registration"){
    val examplesPath = graphBuildingExamplesPath +  "subTyping/"

    scenario("simple case"){
      val p = "regularSuperType"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java") {
        val superClass = fullName2id(s"$p.A")
        val subClass = fullName2id(s"$p.B")

        assert( graph.isa(subClass, superClass) )

      }
    }

    scenario("generic super type"){
      val p = "genericSuperType"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java") {
        val superClass = fullName2id(s"$p.Gen")
        val subClass = fullName2id(s"$p.C")
        val paramType = fullName2id(s"$p.A")

        assert( graph.isa(subClass, superClass) )
        assert( ! graph.isa(subClass, paramType) )
        assert( graph.uses(subClass, paramType) )

      }
    }
  }

  feature("Generic types uses"){
    val examplesPath = graphBuildingExamplesPath +  "genericTypes/"

    scenario("generic type declaration"){
      val p = "genericTypeDecl"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java") {
        val actualParam = fullName2id(s"$p.A")
        val genTypeDeclarant = fullName2id(s"$p.GenTypeDeclarant")
        val user = fullName2id(s"$p.GenTypeDeclarant.user")
        val genType = fullName2id("java.util.List")

        assert( graph.uses(user, genType) )

        assert( graph.uses(user, actualParam) )

        assert( ! graph.uses(genType, actualParam) )
      }
    }

    def numNodesWithFullname(g : DependencyGraph, fullName : String) : Int =
      g.concreteNodesId.count(g.fullName(_) == fullName)


    scenario("generic method declaration"){
      val p = "methodOfGenericType"
      val _ = new ExampleSample(s"$examplesPath/$p/GenColl.java") {

        val actualTypeParam = fullName2id(s"$p.A")
        val formalTypeParam = fullName2id(s"$p.GenColl@T")
        val genType = fullName2id(s"$p.GenColl")
        val genMethod = fullName2id(s"$p.GenColl.put__GenColl@T")
        val userClass = fullName2id(s"$p.User")
        val userMethod = fullName2id(s"$p.User.m__void")

        val genCollNum = numNodesWithFullname(graph, s"$p.GenColl")
        genCollNum shouldBe 1
        val genCollPutNum = numNodesWithFullname(graph, s"$p.GenColl.put")
        genCollPutNum shouldBe 1

        assert( ! graph.uses(genMethod, actualTypeParam) )

        assert( graph.uses(genMethod, formalTypeParam) )

        assert( graph.uses(userMethod, genType) )

        assert( graph.uses(userMethod, actualTypeParam) )

        assert( graph.uses(userMethod, genMethod) )


      }
    }

  }

}
