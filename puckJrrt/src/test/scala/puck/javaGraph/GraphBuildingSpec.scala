package puck
package javaGraph

import puck.graph.constraints.SupertypeAbstraction
import puck.graph._
import puck.util.Debug

//import nodeKind._


class GraphBuildingSpec extends AcceptanceSpec {

  def getDefinition(g : DependencyGraph, nid : NodeId) : NodeId =
    g.getConcreteNode(nid).definition(g).value

  val graphBuildingExamplesPath = Settings.testExamplesPath + "/graphBuilding/"

  feature("constructor registration") {
    val examplesPath = graphBuildingExamplesPath + "constructor/"

    scenario("use of constructor only") {

      val _ = new ScenarioFactory(s"$examplesPath/A.java") {
        val clazz = fullName2id(s"p.A")
        val ctor = fullName2id(s"p.A.A#_void")
        val userDecl = fullName2id(s"p.B.m__void")
        val user = getDefinition(graph, userDecl)

        assert(graph.uses(user, ctor))
        assert(graph.uses(ctor, clazz))

        assert(!graph.uses(user, clazz))
      }
    }
  }
  feature("use registration"){
    val examplesPath = graphBuildingExamplesPath +  "useRegistration/"

    //3 cas de typeUse vers thisClass
    // sig uses : param type
    // body uses : local var or static access
    // thisClass uses : call a sibling method or field
    scenario("this use explicit") {
      val _ = new ScenarioFactory(s"$examplesPath/ThisUseExplicit.java") {
        val clazz = fullName2id("p.A")
        val methM = fullName2id("p.A.m__void")
        val mUserViaThis = fullName2id("p.A.mUserViaThis__void")
        val mUserViaParameter = fullName2id("p.A.mUserViaParameter__A")
        val theParameter = fullName2id("p.A.mUserViaParameter__A.a")


        val mUserViaThisDef = getDefinition(graph, mUserViaThis)
        val mUserViaParameterDef = getDefinition(graph, mUserViaParameter)

        //methodUse
        assert( graph.uses(mUserViaThisDef, methM) )
        assert( ! graph.uses(mUserViaThis, methM) )

        //typeUse
        assert( graph.uses(clazz, clazz) )

        assert( graph.uses(mUserViaParameterDef, methM) )
        assert( !graph.uses(mUserViaParameter, methM) )

        assert( graph.uses(theParameter, clazz) )

      }
    }

    scenario("field type use") {
      val _ = new ScenarioFactory(s"$examplesPath/FieldTypeUse.java") {
        val itc = fullName2id("p.I")
        val field = fullName2id("p.A.field")


        assert( graph.uses(field, itc) )

      }
    }

  }




  feature("typeUse typeMemberUse relation registration"){
    val examplesPath = graphBuildingExamplesPath +  "typeRelationship/"
    scenario("call on field") {
      val _ = new ScenarioFactory(s"$examplesPath/CallOnField.java"){

        val fieldTypeUserDecl = fullName2id("p.A.b")
        val methUserDecl = fullName2id("p.A.ma__void")
        val methUserDef = getDefinition(graph, methUserDecl)

        val typeUsed = fullName2id("p.B")
        val typeMemberUsedDecl = fullName2id("p.B.mb__void")



        val typeUse = graph.getUsesEdge(fieldTypeUserDecl, typeUsed).value
        val typeMemberUse = graph.getUsesEdge(methUserDef, typeMemberUsedDecl).value

        /*println("typeMemberUses2typeUsesMap")
        println(graph.typeMemberUses2typeUsesMap.content.mkString("\n"))
        println("typeUses2typeMemberUsesMap")
        println(graph.typeUses2typeMemberUsesMap.content.mkString("\n"))
        */

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse)

      }


    }

    scenario("call on method's parameter"){
      val _ = new ScenarioFactory(s"$examplesPath/CallOnParameter.java"){

        val mUserDecl = fullName2id("p.A.ma__B")
        val theParameter = fullName2id("p.A.ma__B.b")
        val mUserDef = getDefinition(graph, mUserDecl)
        val classUsed = fullName2id("p.B")
        val mUsed = fullName2id("p.B.mb__void")

        val typeUse = Uses(theParameter, classUsed)
        val typeMemberUse = Uses(mUserDef, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)

      }
    }

    scenario("call from local variable"){
      val _ = new ScenarioFactory(s"$examplesPath/CallOnLocalVariable.java"){

        val mUserDecl = fullName2id("p.A.ma__void")
        val mUserDef = getDefinition(graph, mUserDecl)
        val mUsed = fullName2id("p.B.mb__void")

        val classUsed = fullName2id("p.B")

        val typeUse = Uses(mUserDef, classUsed)
        val typeMemberUse = Uses(mUserDef, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      }
    }

    scenario("chained call"){
      val _ = new ScenarioFactory(s"$examplesPath/ChainedCall.java"){
        val mUserDecl = fullName2id("p.A.ma__void")
        val mUserDef = getDefinition(graph, mUserDecl)
        val mUsed = fullName2id("p.C.mc__void")
        val mIntermediate = fullName2id("p.B.mb__void")
        val classUsed = fullName2id("p.C")

        val typeUse = Uses(mIntermediate, classUsed)
        val typeMemberUse = Uses(mUserDef, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      }
    }
  }

  feature("Abstraction registration"){
    val examplesPath = graphBuildingExamplesPath +  "abstractionRegistration/"
    scenario("one class one interface"){
      val p = "interfaceSupertype"
      val _ = new ScenarioFactory(s"$examplesPath/$p/A.java"){

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
      val _ = new ScenarioFactory(s"$examplesPath/RegularSuperType.java") {
        val superClass = fullName2id("p.A")
        val subClass = fullName2id("p.B")

        assert( graph.isa(subClass, superClass) )

      }
    }

    scenario("generic super type"){
      val _ = new ScenarioFactory(s"$examplesPath/GenericSuperType.java") {
        val superClass = fullName2id("p.Gen")
        val subClass = fullName2id("p.C")
        val paramType = fullName2id("p.A")

        assert( graph.isa(subClass, superClass) )
        assert( ! graph.isa(subClass, paramType) )
        assert( graph.uses(subClass, paramType) )

      }
    }
  }

  feature("Generic types uses"){
    val examplesPath = graphBuildingExamplesPath +  "genericTypes/"

    scenario("generic type declaration"){
      val _ = new ScenarioFactory(s"$examplesPath/GenericTypeDecl.java") {
        val actualParam = fullName2id("p.A")
        val genTypeDeclarant = fullName2id("p.GenTypeDeclarant")
        val user = fullName2id("p.GenTypeDeclarant.user")
        val genType = fullName2id("java.util.List")

        assert( graph.uses(user, genType) )

        assert( graph.uses(user, actualParam) )

        assert( ! graph.uses(genType, actualParam) )
      }
    }

    def numNodesWithFullname(g : DependencyGraph, fullName : String) : Int =
      g.concreteNodesId.count(g.fullName(_) == fullName)


    scenario("generic method declaration"){
      val _ = new ScenarioFactory(s"$examplesPath/MethodOfGenericType.java") {

        val actualTypeParam = fullName2id("p.A")
        val formalTypeParam = fullName2id("p.GenColl@T")
        val genType = fullName2id("p.GenColl")
        val genMethod = fullName2id("p.GenColl.put__GenColl@T")
        val theParameter = fullName2id("p.GenColl.put__GenColl@T.t")
        val userClass = fullName2id("p.User")

        val userMethodDecl = fullName2id("p.User.m__void")
        val userMethodDef = getDefinition(graph, userMethodDecl)

        val genCollNum = numNodesWithFullname(graph, "p.GenColl")
        genCollNum shouldBe 1
        val genCollPutNum = numNodesWithFullname(graph, "p.GenColl.put")
        genCollPutNum shouldBe 1

        assert( ! graph.uses(genMethod, actualTypeParam) )

        assert( graph.uses(theParameter, formalTypeParam) )

        assert( graph.uses(userMethodDef, genType) )

        assert( graph.uses(userMethodDef, actualTypeParam) )

        assert( graph.uses(userMethodDef, genMethod) )


      }
    }

    scenario("generic - type relationship"){
      val _ = new ScenarioFactory(s"$examplesPath/TypeRelationship.java") {

        val actualTypeParam = fullName2id("p.A")
        val actualTypeParamMethod = fullName2id("p.A.m__void")

        val fieldDeclarant = fullName2id("p.B.la")

        val userClass = fullName2id("p.B")
        val userMethodDecl = fullName2id("p.B.mUser__void")
        val userMethodDef = getDefinition(graph, userMethodDecl)

        val genericMethod = fullName2id("java.util.List.get__int")

        val fieldTypeUse = graph.getUsesEdge(fieldDeclarant, actualTypeParam).value
        val typeMemberUse = graph.getUsesEdge(userMethodDef, actualTypeParamMethod).value
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
      val _ = new ScenarioFactory(s"$examplesPath/A.java") {
        val field = fullName2id(s"p.A.f")
        val getterDecl = fullName2id(s"p.A.getF__void")
        val getter = getDefinition(graph, getterDecl)

        val setterDecl = fullName2id(s"p.A.setF__int")
        val setter = getDefinition(graph, setterDecl)

        val inc0Decl = fullName2id(s"p.A.incF0__void")
        val inc1Decl = fullName2id(s"p.A.incF1__void")
        val inc2Decl = fullName2id(s"p.A.incF2__void")
        val dec0Decl = fullName2id(s"p.A.decF0__void")
        val dec1Decl = fullName2id(s"p.A.decF1__void")

        val inc0 = getDefinition(graph, inc0Decl)
        val inc1 = getDefinition(graph, inc1Decl)
        val inc2 = getDefinition(graph, inc2Decl)
        val dec0 = getDefinition(graph, dec0Decl)
        val dec1 = getDefinition(graph, dec1Decl)

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
