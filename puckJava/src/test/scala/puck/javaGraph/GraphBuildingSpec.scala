package puck
package javaGraph

//import nodeKind._
import puck.graph._
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

        val typeUse = graph.getUsesEdge(fieldTypeUser, typeUsed).value
        val typeMemberUse = graph.getUsesEdge(methUser, typeMemberUsed).value

        /*println("typeMemberUses2typeUsesMap")
        println(graph.typeMemberUses2typeUsesMap.content.mkString("\n"))
        println("typeUses2typeMemberUsesMap")
        println(graph.typeUses2typeMemberUsesMap.content.mkString("\n"))

        quickFrame(graph)*/
        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse)

      }


    }

    scenario("call on method's parameter"){
      val p = "callOnParameter"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val mUser = fullName2id(s"$p.A.ma__B")
        val classUsed = fullName2id(s"$p.B")
        val mUsed = fullName2id(s"$p.B.mb__void")

        val typeUse = DGEdge.UsesK(mUser, classUsed)
        val typeMemberUse = DGEdge.UsesK(mUser, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)

      }
    }

    scenario("call from local variable"){
      val p = "callOnLocalVariable"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java"){

        val mUser = fullName2id(s"$p.A.ma__void")
        val mUsed = fullName2id(s"$p.B.mb__void")

        val classUsed = fullName2id(s"$p.B")

        val typeUse = DGEdge.UsesK(mUser, classUsed)
        val typeMemberUse = DGEdge.UsesK(mUser, mUsed)

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

        val typeUse = DGEdge.UsesK(mIntermediate, classUsed)
        val typeMemberUse = DGEdge.UsesK(mUser, mUsed)

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

        graph.abstractions(classUsed) should contain ( AccessAbstraction(superType, SupertypeAbstraction) )
        graph.abstractions(mUsed) should contain ( AccessAbstraction(absmUsed, SupertypeAbstraction) )
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

    scenario("generic - type relationship"){
      val p = "typeRelationship"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java") {

        val actualTypeParam = fullName2id(s"$p.A")
        val actualTypeParamMethod = fullName2id(s"$p.A.m__void")

        val fieldDeclarant = fullName2id(s"$p.B.la")

        val userClass = fullName2id(s"$p.B")
        val userMethod = fullName2id(s"$p.B.mUser__void")

        val genericMethod = fullName2id("java.util.List.get__int")

        val fieldTypeUse = graph.getUsesEdge(fieldDeclarant, actualTypeParam).value
        val typeMemberUse = graph.getUsesEdge(userMethod, actualTypeParamMethod).value
        val parTypeUse = graph.getUsesEdge(genericMethod, actualTypeParam).value


        assert( fieldTypeUse existsIn graph )
        assert( typeMemberUse existsIn graph )

        graph.typeMemberUsesOf(fieldTypeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (parTypeUse)

      }
    }

  }

  feature("Read/Write uses"){
    val examplesPath = graphBuildingExamplesPath +  "readWrite/"

    scenario("generic type declaration"){
      val _ = new ExampleSample(s"$examplesPath/A.java") {
        val field = fullName2id(s"p.A.f")
        val getter = fullName2id(s"p.A.getF__void")
        val setter = fullName2id(s"p.A.setF__int")
        val inc0 = fullName2id(s"p.A.incF0__void")
        val inc1 = fullName2id(s"p.A.incF1__void")
        val inc2 = fullName2id(s"p.A.incF2__void")
        val dec0 = fullName2id(s"p.A.decF0__void")
        val dec1 = fullName2id(s"p.A.decF1__void")

        assert( graph.uses(getter, field) )
        graph.usesAccessKind(getter, field) shouldBe Some(Read)

        assert( graph.uses(setter, field) )
        graph.usesAccessKind(setter, field) shouldBe Some(Write)

        assert( graph.uses(inc0, field) )
        graph.usesAccessKind(inc0, field) shouldBe Some(RW)

        assert( graph.uses(inc1, field) )
        graph.usesAccessKind(inc1, field) shouldBe Some(RW)

        assert( graph.uses(inc2, field) )
        graph.usesAccessKind(inc2, field) shouldBe Some(RW)

        assert( graph.uses(dec0, field) )
        graph.usesAccessKind(dec0, field) shouldBe Some(RW)

        assert( graph.uses(dec1, field) )
        graph.usesAccessKind(dec1, field) shouldBe Some(RW)

      }
    }
  }
}