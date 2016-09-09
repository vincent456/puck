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

import puck.AcceptanceSpec
import puck.graph.{NamedType, NodeIdP, ParameterizedType}
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 11/04/16.
  */
class BindingRelationshipSpec extends AcceptanceSpec {

  scenario("call on field") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class B { public void mb(){} }
        |
        |class A {
        |
        |    private B b;
        |
        |    public void ma(){ b.mb(); }
        |}"""){

      val typeUse : NodeIdP = ("p.A.b", "p.B")
      val typeMemberUse : NodeIdP = ("p.A.ma().Definition", "p.B.mb()")


      graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      graph.typeUsesOf(typeMemberUse) should contain (typeUse)

    }


  }

  scenario("call on method's parameter"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class B { public void mb(){} }
        |
        |class A { public void ma(B b){ b.mb(); } }"""
    ){



      val typeUse : NodeIdP = ("p.A.ma(B).b", "p.B")
      val typeMemberUse : NodeIdP = ("p.A.ma(B).Definition", "p.B.mb()")

      graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      graph.typeUsesOf(typeMemberUse) should contain (typeUse)

    }
  }

  scenario("call on local variable"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {
        |    public void ma() {
        |        B b = new B();
        |        b.mb();
        |    }
        |}
        |
        |class B { public void mb(){} }"""){



      val typeUse : NodeIdP = ("p.A.ma().Definition.b", "p.B")
      val typeMemberUse : NodeIdP = ("p.A.ma().Definition", "p.B.mb()")

      graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      graph.typeUsesOf(typeMemberUse) should contain (typeUse)
    }
  }

  scenario("chained call"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {
        |    public void ma() {
        |        B b = new B();
        |        b.mb().mc();
        |    }
        |}
        |
        |class B { public C mb(){ return new C();} }
        |
        |class C { public void mc(){} }"""
    ){

      val typeUse : NodeIdP = ("p.B.mb()", "p.C")
      val typeMemberUse : NodeIdP = ("p.A.ma().Definition", "p.C.mc()")

      graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      graph.typeUsesOf(typeMemberUse) should contain (typeUse)
    }
  }

  scenario("cond expr"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A { void m(){ System.out.println("A.m()"); } }
        |class Cond {
        |    void condM(boolean c, A a) {  (c ? new A() : a).m(); }
        |}"""){


      val typeUse : NodeIdP = ("p.Cond.condM(boolean,A).a", "p.A")
      val typeUse1 : NodeIdP = ("p.A.A()", "p.A")
      val typeMemberUse : NodeIdP = ("p.Cond.condM(boolean,A).Definition", "p.A.m()")

      graph.typeMemberUsesOf(typeUse) should contain (typeMemberUse)
      graph.typeUsesOf(typeMemberUse) should contain (typeUse)

      graph.typeMemberUsesOf(typeUse1) should contain (typeMemberUse)
      graph.typeUsesOf(typeMemberUse) should contain (typeUse1)

    }
  }

  scenario("generic - type parameter as method return type"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T> {  T get(){return null;} }
        |
        |class A{ void m(){} }
        |
        |class B {
        |    Wrapper<A> wa = new Wrapper<A>();
        |
        |    void doM(){ wa.get().m(); }
        |}"""
    ){

      graph.styp("p.B.wa").value should be (ParameterizedType("p.Wrapper", List(NamedType("p.A"))))

      graph.typeUsesOf(("p.B.doM().Definition", "p.Wrapper.get()")) should contain (("p.B.wa", "p.Wrapper") : NodeIdP)
      graph.typeUsesOf(("p.B.doM().Definition", "p.Wrapper.get()")) should not contain (("p.B.wa", "p.A") : NodeIdP)

      graph.typeUsesOf(("p.B.doM().Definition", "p.A.m()")) should contain (("p.B.wa", "p.A") : NodeIdP)
      graph.typeUsesOf(("p.B.doM().Definition", "p.A.m()")) should not contain (("p.B.wa", "p.Wrapper") : NodeIdP )

      graph.typeMemberUsesOf(("p.B.wa", "p.A")) should contain (("p.B.doM().Definition", "p.A.m()") : NodeIdP)
      graph.typeMemberUsesOf(("p.B.wa", "p.A")).size should be (1)

      //graph.typeMemberUsesOf(("p.B.wa", "p.A")) should not contain ("p.B.doM().Definition", "p.Wrapper.get()" : NodeIdP)
    }
  }

  scenario("generic - type parameter as argument type"){
    val _ = new ScenarioFactory(
      s"""package fileSystem;
          |
          |import java.util.ArrayList;
          |import java.util.List;
          |
          |class Directory {
          |   private List<Directory> directories = new ArrayList<Directory>();
          |   public void add( Directory d ) { directories.add( d ); }
          |}"""
    ){

      graph.typeMemberUsesOf(("fileSystem.Directory.directories", "java.util.List")) should contain (
        ("fileSystem.Directory.add(Directory).Definition", "java.util.List.add(E)") : NodeIdP)

      graph.typeMemberUsesOf(("fileSystem.Directory.directories", "java.util.List")).size should be (1)

      graph.typeMemberUsesOf(("fileSystem.Directory.directories", "fileSystem.Directory")) shouldBe empty

    }
  }

  scenario("generic - foreach"){
    val _ = new ScenarioFactory(
      s"""package fileSystem;
          |
          |import java.util.ArrayList;
          |import java.util.List;
          |
          |class Directory {
          |   private String    name;
          |   private List<Directory> directories = new ArrayList<Directory>();
          |
          |   public void display(String path) {
          |      System.out.println(path + name);
          |      String npath = path + name +"/";
          |      for(Directory d: directories)
          |         d.display(npath);
          |   }
          |}"""
    ){
      graph.typeMemberUsesOf(("fileSystem.Directory.directories", "java.util.List")) shouldBe empty
    }
  }

  scenario("use of method of type a variable value instanciated via subtyping"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T>{  T getT(){ return null; } }
        |
        |class C { void execute(){} }
        |
        |class WC extends Wrapper<C> {
        |    public void m() { getT().execute(); }
        |}"""
    ){


      assert( graph.uses("p.WC", "p.C"))
      assert( graph.uses("p.WC.m().Definition", "p.C.execute()"))

      val classTypeUse : NodeIdP = ("p.WC", "p.C")
      val useOfExecute : NodeIdP = ("p.WC.m().Definition", "p.C.execute()")

      graph.typeUsesOf(useOfExecute) should contain (classTypeUse)
      graph.typeMemberUsesOf(classTypeUse) should contain (useOfExecute)
      graph.typeMemberUsesOf(classTypeUse).size should be (1)

    }
  }

  scenario("Use of parameterized method where Type variable instantiated via subclassing"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class ListModel<E>{
        |    E e;
        |    E getE(){ return e;}
        |}
        |class HasElement extends ListModel<String> {}
        |
        |public class C {
        |
        |    HasElement data;
        |
        |    public void m() { data.getE().toString(); }
        |
        |}"""
    ){
      val mDef = fullName2id("p.C.m().Definition")
      val getE = fullName2id("p.ListModel.getE()")

      val hasElement = fullName2id("p.HasElement")
      val string = fullName2id("java.lang.String")
      val toString_ = fullName2id("java.lang.String.toString()")


      assert(graph.uses(mDef, getE))
      assert(graph.uses(mDef, toString_))

      graph.typeUsesOf(mDef, toString_) should contain ((hasElement, string))
    }
  }


  scenario("Use of parameterized method with RAW Type variable (instantiated) via subclassing"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class ListModel<E>{
        |    E e;
        |    E getE(){ return e;}
        |}
        |class HasElement extends ListModel {}
        |
        |public class C {
        |
        |    HasElement data;
        |
        |    public void m() { data.getE().toString(); }
        |
        |}"""
    ){
      val mDef = fullName2id("p.C.m().Definition")
      val getE = fullName2id("p.ListModel.getE()")

      val hasElement = fullName2id("p.HasElement")
      val obj = fullName2id("java.lang.Object")
      val toString_ = fullName2id("java.lang.Object.toString()")


      assert(graph.uses(mDef, getE))
      assert(graph.uses(mDef, toString_))

      graph.typeUsesOf(mDef, toString_) should contain ((hasElement, obj))
    }
  }

  scenario("Use of parameterized method where Type variable instantiated via interface subtyping"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |interface ListModel<E>{  E getE(); }
        |interface HasElement extends ListModel<String> {}
        |
        |public class C {
        |
        |    HasElement data;
        |
        |    public void m() { data.getE().toString(); }
        |
        |}"""
    ){
      val mDef = fullName2id("p.C.m().Definition")
      val getE = fullName2id("p.ListModel.getE()")

      val hasElement = fullName2id("p.HasElement")
      val string = fullName2id("java.lang.String")
      val toString_ = fullName2id("java.lang.String.toString()")


      assert(graph.uses(mDef, getE))
      assert(graph.uses(mDef, toString_))

      graph.typeUsesOf(mDef, toString_) should contain ((hasElement, string))
    }
  }

  scenario("Use of parameterized method with RAW Type variable (instantiated) via interface subtyping"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |interface ListModel<E>{ E getE(); }
        |interface HasElement extends ListModel {}
        |
        |public class C {
        |
        |    HasElement data;
        |
        |    public void m() { data.getE().toString(); }
        |
        |}"""
    ){

      assert(graph.uses("p.C.m().Definition", "p.ListModel.getE()"))
      assert(graph.uses("p.C.m().Definition", "java.lang.Object.toString()"))
      graph.typeUsesOf("p.C.m().Definition", "java.lang.Object.toString()") should contain (("p.HasElement", "java.lang.Object") : NodeIdP)
    }
  }

}
