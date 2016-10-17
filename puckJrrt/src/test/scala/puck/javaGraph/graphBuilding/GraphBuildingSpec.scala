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
import puck.javaGraph.ScenarioFactory
import puck.AcceptanceSpec


class GraphBuildingSpec extends AcceptanceSpec {


  feature("Abstraction registration"){
    ignore("one class one interface : ignore because now supertype abstraction are dinamcally computed"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |interface SuperType { void ma(); }
          |class A implements SuperType{ public void ma(){} }"""){

        graph.abstractions("p.A") should contain ( AccessAbstraction("p.SuperType", SupertypeAbstraction) )
        graph.abstractions("p.A.ma()") should contain ( AccessAbstraction("p.SuperType.ma()", SupertypeAbstraction) )
      }
    }

    scenario("getter/setter") {
      val s = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    int f = 42;
          |
          |    void setF(int f){ this.f = f; }
          |    int getF(){ return f; }
          |
          |}""")

      import s._

      graph.abstractions("p.A.f") should contain (ReadWriteAbstraction(Some("p.A.getF()"), Some("p.A.setF(int)")))

    }
  }


  feature("Isa registration"){
    scenario("simple case"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {}
          |
          |class B extends A {}""") {

        assert( graph.isa("p.B", "p.A") )

      }
    }

    scenario("generic super type"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {}
          |
          |class Gen<T> {}
          |
          |class C extends Gen<A> {}""") {
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

    scenario("array decl"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A { int[] is; }"""
      ) {
        val field = fullName2id("p.A.is")
        val int = fullName2id("@primitive.int")
        val array = fullName2id("@primitive.[]")


        graph.styp(field).value should be (ParameterizedType(array, List(NamedType(int))))


      }
    }
    scenario("array usage"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |    void m(){}
          |
          |    int i = 0;
          |
          |    void arrayUser(A[] as){ as[i].m(); }
          |}
        """.stripMargin) {
        val arrayUser = fullName2id("p.A.arrayUser(A[]).Definition")
        val m = fullName2id("p.A.m()")
        val i = fullName2id("p.A.i")
        val array = fullName2id("@primitive.[]")

        assert( graph.uses(arrayUser, m) )
        assert( graph.uses(arrayUser, i) )
      }
    }
    scenario("generic type declaration"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |import java.util.List;
          |
          |class A {}
          |
          |class GenTypeDeclarant { private List<A> user; }"""
      ) {
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


    scenario("method of generic type declaration"){
      val s = new ScenarioFactory(
        """package p;
          |
          |import java.util.List;
          |
          |class A{}
          |
          |class User {
          |
          |    public void m(){
          |        GenColl<A> colla = new GenColl<A>();
          |        colla.put(new A());
          |    }
          |
          |}
          |
          |class GenColl<T> {
          |    public void put(T t){}
          |}"""
      )
      import s._
        numNodesWithFullname(graph, "p.GenColl") shouldBe 1
        numNodesWithFullname(graph, "p.GenColl.put") shouldBe 1

        assert( ! graph.uses("p.GenColl.put(T).t", "p.A") )

        assert( graph.uses("p.GenColl.put(T).t", "p.GenColl@T") )

        assert( ! graph.uses("p.User.m().Definition", "p.A") )

        //printFullNames()
        assert( graph.typ("p.User.m().Definition.0") == ParameterizedType("p.GenColl", List(NamedType("p.A"))) )



        assert( graph.uses("p.User.m().Definition", "p.GenColl.put(T)") )



    }


    scenario("upper bounded wildcard"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {}
          |
          |class B<T>{}
          |
          |class C {
          |    B<? super A> lowerBounded;
          |    B<? extends A> upperBounded;
          |}"""
      ) {
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
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {}
          |
          |class B<T>{}
          |
          |class C {
          |    B<? super A> lowerBounded;
          |    B<? extends A> upperBounded;
          |}"""
      ) {
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

    scenario("generic type declaration"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |    int f = 0;
          |
          |    public int getF() { return f; }
          |
          |    public void setF(int f) { this.f = f; }
          |
          |    public void incF0() { this.f = f + 1; }
          |
          |    public void incF1() { f++; }
          |
          |    public void incF2() { ++f; }
          |
          |    public void decF0() { --f; }
          |    public void decF1() { f--; }
          |}"""
      ) {
        val a : NodeId = "p.A"
        val field  : NodeId = "p.A.f"
        val getter = fullName2id(s"p.A.getF().Definition")

        val setter = fullName2id(s"p.A.setF(int).Definition")

        val inc0 = fullName2id(s"p.A.incF0().Definition")
        val inc1 = fullName2id(s"p.A.incF1().Definition")
        val inc2 = fullName2id(s"p.A.incF2().Definition")
        val dec0 = fullName2id(s"p.A.decF0().Definition")
        val dec1 = fullName2id(s"p.A.decF1().Definition")


        assert( graph.uses(getter, field) )
        graph.usesAccessKind( ((a,a), (getter, field)) ) shouldBe Some(Read)

        assert( graph.uses(setter, field) )
        graph.usesAccessKind( ((a,a), (setter, field)) ) shouldBe Some(Write)

        assert( graph.uses(inc0, field) )
        graph.usesAccessKind( ((a,a), (inc0, field)) ) shouldBe Some(RW)

        assert( graph.uses(inc1, field) )
        graph.usesAccessKind( ((a,a), (inc1, field)) ) shouldBe Some(RW)

        assert( graph.uses(inc2, field) )
        graph.usesAccessKind( ((a,a), (inc2, field)) ) shouldBe Some(RW)

        assert( graph.uses(dec0, field) )
        graph.usesAccessKind( ((a,a), (dec0, field)) ) shouldBe Some(RW)

        assert( graph.uses(dec1, field) )
        graph.usesAccessKind( ((a,a), (dec1, field)) ) shouldBe Some(RW)

      }
    }
  }

  feature("anonymous class"){

    scenario("anonymous class instanciated in local variable") {
      val s = new ScenarioFactory(
        """package p;
          |
          |interface DoM { void m(); }
          |
          |class A {
          |
          |    void ma(){
          |        DoM d2 = new DoM(){
          |            public void m(){ System.out.println("also do m !"); }
          |        };
          |    }
          |}""")
      import s._

      assert( graph.contains("p.A.ma().Definition", "p.A.ma().Anonymous0") )

    }

    scenario("static initializer declaring array list") {
      val s = new ScenarioFactory(
        """package p;
          |
          |class G<T> {};
          |public class C {
          |	  {	G<String> g = new G<String>(); }
          |}""")
    }

    scenario("anonymous class instanciated in field") {
      val s = new ScenarioFactory(
        """package p;
          |
          |interface DoM { void m(); }
          |
          |class A {
          |
          |    DoM f = new DoM(){
          |        public void m(){ System.out.println("do m !"); }
          |    };
          |
          |}""")

      import s._

      assert( graph.contains("p.A.f.Definition", "p.A.f.Anonymous0") )
    }
  }


}
