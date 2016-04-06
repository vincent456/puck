/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.javaGraph.graphBuilding


import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.ScenarioFactory
import puck.{AcceptanceSpec, Settings}


class GraphBuildingSpec extends AcceptanceSpec {

  val graphBuildingExamplesPath = Settings.testExamplesPath + "/graphBuilding/"

  feature("contains registration"){

    scenario("static class member") {
      val _ = new ScenarioFactory(
        s"$graphBuildingExamplesPath/staticClassMember/A.java") {

        val p = fullName2id("p")
        val classA = fullName2id("p.A")
        val innerA = fullName2id("p.A.InnerA")
        val innerACtor = fullName2id("p.A.InnerA.InnerA()")
        val innerACtorDef = fullName2id("p.A.InnerA.InnerA().Definition")

        graph.container(classA).value shouldBe p
        graph.content(p) should contain (classA)

        graph.container(innerA).value shouldBe classA
        graph.content(classA) should contain (innerA)

        graph.container(innerACtor).value shouldBe innerA
        graph.content(innerA) should contain (innerACtor)

        graph.container(innerACtorDef).value shouldBe innerACtor
        graph.content(innerACtor) should contain (innerACtorDef)

      }

    }

    scenario("instance class member") {
      val _ = new ScenarioFactory(
        s"$graphBuildingExamplesPath/instanceClassMember/A.java") {

        val p = fullName2id("p")
        val classA = fullName2id("p.A")
        val innerA = fullName2id("p.A.InnerA")
        val innerACtor = fullName2id("p.A.InnerA.InnerA()")
        val innerACtorDef = fullName2id("p.A.InnerA.InnerA().Definition")

        graph.container(classA).value shouldBe p
        graph.content(p) should contain (classA)

        graph.container(innerA).value shouldBe classA
        graph.content(classA) should contain (innerA)

        graph.container(innerACtor).value shouldBe innerA
        graph.content(innerA) should contain (innerACtor)

        graph.container(innerACtorDef).value shouldBe innerACtor
        graph.content(innerACtor) should contain (innerACtorDef)

      }

    }

    scenario("instance class declared in static method") {
      val _ = new ScenarioFactory(
        s"$graphBuildingExamplesPath/namedClassDeclaredInStaticMethod/A.java") {
        val p = fullName2id("p")
        val classA = fullName2id("p.A")
        val meth = fullName2id("p.A.declareInnerClass()")
        val methDef = fullName2id("p.A.declareInnerClass().Definition")

        val innerClass = fullName2id("p.A.declareInnerClass().CanDoMInstance")

        graph.container(classA).value shouldBe p
        graph.content(p) should contain (classA)

        graph.container(meth).value shouldBe classA
        graph.content(classA) should contain (meth)

        graph.container(methDef).value shouldBe meth
        graph.content(meth) should contain (methDef)

        graph.container(innerClass).value shouldBe methDef
        graph.content(methDef) should contain (innerClass)
      }

    }

    scenario("generic classes variable"){
      val _ = new ScenarioFactory(
        s"$graphBuildingExamplesPath/typeVariables/ClassVariable.java") {

        val classA = fullName2id("p.A")
        val classB = fullName2id("p.B")

        val ta =fullName2id("p.A@T")
        val tb =fullName2id("p.B@T")
        graph.contains(classA, ta)
        graph.contains(classB, tb)

      }
    }

    scenario("generic method variable"){
      val _ = new ScenarioFactory(
        s"$graphBuildingExamplesPath/typeVariables/MethodTypeVariable.java") {

        val a = fullName2id("p.A")
        val at = fullName2id("p.A@T")

        val m1 = fullName2id("p.A.castMe(Object)")
        val m2 = fullName2id("p.A.castMeInstead(Object)")

        val mt1 =fullName2id("p.A.castMe(Object)@T")
        val mt2 =fullName2id("p.A.castMeInstead(Object)@T")
        graph.contains(a, at)
        graph.contains(m1, mt1)
        graph.contains(m2, mt2)
      }
    }

    scenario("up bounded type variable"){
      val _ = new ScenarioFactory(s"$graphBuildingExamplesPath/typeVariables/NamingProblem.java") {
        val u = fullName2id("p.Comparator.thenComparing(Comparator)@U")
        val param = fullName2id("p.Comparator.thenComparing(Comparator).keyComparator")
        val m = fullName2id("p.Comparator.thenComparing(Comparator)")

        graph.contains(m, u)
        graph.uses(param, u)

      }
    }

    info("method m(A...) recognized as m(A[])")
    scenario("overloading with variadic method"){
      val _ = new ScenarioFactory(s"$graphBuildingExamplesPath/variadicMethod/A.java"){
        val m1name = "p.A.m(double)"
        val m1id = fullName2id(m1name)
        val m2name = "p.A.m(double[])"
        val m2id = fullName2id(m2name)

        import ShowDG._
        (graph, m1id).shows(sigFullName) shouldBe m1name
        (graph, m2id).shows(sigFullName) shouldBe m2name

      }
    }
  }

  feature("typeUse typeMemberUse relation registration"){
    val examplesPath = graphBuildingExamplesPath +  "typeRelationship/"

    scenario("call on field") {
      val _ = new ScenarioFactory(s"$examplesPath/CallOnField.java"){

        val fieldTypeUserDecl = fullName2id("p.A.b")
        val methUserDef = fullName2id("p.A.ma().Definition")

        val typeUsed = fullName2id("p.B")
        val typeMemberUsedDecl = fullName2id("p.B.mb()")


        val typeUse = graph.getUsesEdge(fieldTypeUserDecl, typeUsed).value
        val typeMemberUse = graph.getUsesEdge(methUserDef, typeMemberUsedDecl).value

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse)

      }


    }

    scenario("call on method's parameter"){
      val _ = new ScenarioFactory(s"$examplesPath/CallOnParameter.java"){

        val theParameter = fullName2id("p.A.ma(B).b")
        val mUser = fullName2id("p.A.ma(B).Definition")
        val classUsed = fullName2id("p.B")
        val mUsed = fullName2id("p.B.mb()")

        val typeUse = Uses(theParameter, classUsed)
        val typeMemberUse = Uses(mUser, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse)

      }
    }

    scenario("call on local variable"){
      val _ = new ScenarioFactory(s"$examplesPath/CallOnLocalVariable.java"){

        val mUser = fullName2id("p.A.ma().Definition")
        val mUsed = fullName2id("p.B.mb()")

        val classUsed = fullName2id("p.B")

        val typeUse = Uses(mUser, classUsed)
        val typeMemberUse = Uses(mUser, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse)
      }
    }

    scenario("chained call"){
      val _ = new ScenarioFactory(s"$examplesPath/ChainedCall.java"){
        val mUser = fullName2id("p.A.ma().Definition")
        val mUsed = fullName2id("p.C.mc()")
        val mIntermediate = fullName2id("p.B.mb()")
        val classUsed = fullName2id("p.C")

        val typeUse = Uses(mIntermediate, classUsed)
        val typeMemberUse = Uses(mUser, mUsed)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse)
      }
    }

    scenario("cond expr"){
      val _ = new ScenarioFactory(s"$examplesPath/ConditionalExprCase.java"){

        val condM = fullName2id("p.Cond.condM(boolean,A).Definition")
        val m = fullName2id("p.A.m()")

        val paramA = fullName2id("p.Cond.condM(boolean,A).a")
        val ctorA = fullName2id("p.A.A()")
        val classA = fullName2id("p.A")

        val typeUse = Uses(paramA, classA)
        val typeUse1 = Uses(ctorA, classA)
        val typeMemberUse = Uses(condM, m)

        graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse)

        graph.typeMemberUsesOf(typeUse1) should contain (typeMemberUse)
        graph.typeUsesOf(typeMemberUse) should contain (typeUse1)

      }
    }


  }

  feature("Abstraction registration"){
    val examplesPath = graphBuildingExamplesPath +  "abstractionRegistration/"
    scenario("one class one interface"){
      val p = "interfaceSupertype"
      val _ = new ScenarioFactory(s"$examplesPath/$p/A.java"){

        val classUsed = fullName2id(s"$p.A")
        val mUsed = fullName2id(s"$p.A.ma()")
        val superType = fullName2id(s"$p.SuperType")
        val absmUsed = fullName2id(s"$p.SuperType.ma()")

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

    scenario("array decl"){
      val _ = new ScenarioFactory(s"$examplesPath/ArrayDecl.java") {
        val field = fullName2id("p.A.is")
        val int = fullName2id("@primitive.int")
        val array = fullName2id("@primitive.[]")


        graph.styp(field).value should be (ParameterizedType(array, List(NamedType(int))))


      }
    }
    scenario("array usage"){
      val _ = new ScenarioFactory(s"$examplesPath/ArrayUsage.java") {
        val arrayUser = fullName2id("p.A.arrayUser(A[]).Definition")
        val m = fullName2id("p.A.m()")
        val i = fullName2id("p.A.i")
        val array = fullName2id("@primitive.[]")

        assert( graph.uses(arrayUser, m) )
        assert( graph.uses(arrayUser, i) )
      }
    }
    scenario("generic type declaration"){
      val _ = new ScenarioFactory(s"$examplesPath/GenericTypeDecl.java") {
        val actualParam = fullName2id("p.A")
        val genTypeDeclarant = fullName2id("p.GenTypeDeclarant")
        val user = fullName2id("p.GenTypeDeclarant.user")
        val genType = fullName2id("java.util.List")

        graph.styp(user).value should be (ParameterizedType(genType, List(NamedType(actualParam))))

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
        val genMethod = fullName2id("p.GenColl.put(T)")
        val theParameter = fullName2id("p.GenColl.put(T).t")
        val userClass = fullName2id("p.User")

        val userMethodDef = fullName2id("p.User.m().Definition")

       numNodesWithFullname(graph, "p.GenColl") shouldBe 1
       numNodesWithFullname(graph, "p.GenColl.put") shouldBe 1

        assert( ! graph.uses(genMethod, actualTypeParam) )

        assert( graph.uses(theParameter, formalTypeParam) )

        assert( graph.uses(userMethodDef, genType) )

        assert( graph.uses(userMethodDef, actualTypeParam) )

        assert( graph.uses(userMethodDef, genMethod) )


      }
    }




    scenario("upper bounded wildcard"){
      val _ = new ScenarioFactory(s"$examplesPath/Wildcard.java") {
        val upperBoundedField = fullName2id("p.C.upperBounded")
        val bound = fullName2id("p.A")
        val genType = fullName2id("p.B")

        assert(graph uses (upperBoundedField, genType))
        assert(graph uses (upperBoundedField, bound))
        val t = ParameterizedType(genType, List(Covariant(NamedType(bound))) )
        graph.styp(upperBoundedField).value should be (t)

      }
    }

    scenario("lower bounded wildcard"){
      val _ = new ScenarioFactory(s"$examplesPath/Wildcard.java") {
        val lowerBoundedField = fullName2id("p.C.lowerBounded")
        val bound = fullName2id("p.A")
        val genType = fullName2id("p.B")

        assert(graph uses (lowerBoundedField, genType))
        assert(graph uses (lowerBoundedField, bound))
        val t = ParameterizedType(genType, List(Contravariant(NamedType(bound))) )
        graph.styp(lowerBoundedField).value should be (t)

      }
    }
  }

  feature("Read/Write uses"){
    val examplesPath = graphBuildingExamplesPath +  "readWrite/"

    scenario("generic type declaration"){
      val _ = new ScenarioFactory(s"$examplesPath/A.java") {
        val field = fullName2id(s"p.A.f")
        val getter = fullName2id(s"p.A.getF().Definition")

        val setter = fullName2id(s"p.A.setF(int).Definition")

        val inc0 = fullName2id(s"p.A.incF0().Definition")
        val inc1 = fullName2id(s"p.A.incF1().Definition")
        val inc2 = fullName2id(s"p.A.incF2().Definition")
        val dec0 = fullName2id(s"p.A.decF0().Definition")
        val dec1 = fullName2id(s"p.A.decF1().Definition")


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

  feature("anonymous class"){
    val examplesPath = graphBuildingExamplesPath +  "anonymousClass/"

    scenario("anonymous class instanciated in local variable") {
      val _ = new ScenarioFactory(s"$examplesPath/AnonymousClassLocalVariable.java") {
        val mDef = fullName2id(s"p.A.ma().Definition")
        val anonymousClass = fullName2id(s"p.A.ma().Anonymous0")

        assert( graph.contains(mDef, anonymousClass) )
      }
    }

    scenario("anonymous class instanciated in field") {
      val _ = new ScenarioFactory(s"$examplesPath/AnonymousClassField.java") {

        val field = fullName2id(s"p.A.f.Definition")
        val anonymousClass = fullName2id(s"p.A.f.Anonymous0")
        assert( graph.contains(field, anonymousClass) )


      }
    }
  }
}
