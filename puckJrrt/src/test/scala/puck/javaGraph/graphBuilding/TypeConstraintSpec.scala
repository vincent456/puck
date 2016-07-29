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
  * Created by Loïc Girault on 06/04/16.
  */
class TypeConstraintSpec extends AcceptanceSpec {

  scenario("Field init") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {}
        |
        |class B {  A a = new A(); } """
    ) {
      graph.usesThatShouldUsesASubtypeOf(("p.B.a", "p.A")) should contain ( ("p.A.A()", "p.A") : NodeIdP)
    }
  }

  scenario("Field assignment") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {  }
        |
        |class B {  A a;  void m(){ a = new A(); }  } """
    ) {
      graph.usesThatShouldUsesASubtypeOf(("p.B.a", "p.A")) should contain ( ("p.A.A()", "p.A") : NodeIdP)
    }
  }

  scenario("generic - type uses  FieldDecl constraint "){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T> { T get(){return null;} }
        |
        |class A{ void m(){} }
        |
        |class B {  Wrapper<A> wa = new Wrapper<A>(); } """
    ){
      assert(graph uses ("p.B.wa.Definition", "p.A"))

      graph.styp("p.B.wa").value should be (ParameterizedType("p.Wrapper", List(NamedType("p.A"))))

      graph.usesThatShouldUsesSameTypeAs(("p.B.wa", "p.A")) should contain ( ("p.B.wa.Definition", "p.A") : NodeIdP )
      graph.usesThatShouldUsesASuperTypeOf(("p.Wrapper.Wrapper()", "p.Wrapper")) should contain ( ("p.B.wa", "p.Wrapper") : NodeIdP )
      graph.usesThatShouldUsesASubtypeOf(("p.B.wa", "p.Wrapper")) should contain (  ("p.Wrapper.Wrapper()", "p.Wrapper") : NodeIdP)

    }
  }

  ignore("generic - type uses  VarDecl constraint "){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T> { T get(){return null;} }
        |
        |class A{ void m(){} }
        |
        |class B { void mb(){ Wrapper<A> wa = new Wrapper<A>(); } }"""
    ){

      assert(graph uses ("p.B.mb().Definition", "p.A"))

//      graph.usesThatShouldUsesSameTypeAs(("p.B.mb.Definition", "p.A")) should not contain ( ("p.B.mb.Definition", "p.A") : NodeIdP )
//      graph.usesThatShouldUsesASuperTypeOf(("p.Wrapper.Wrapper()", "p.Wrapper")) should contain ( ("p.B.wa", "p.Wrapper") : NodeIdP )
//      graph.usesThatShouldUsesASubtypeOf(("p.B.wa", "p.Wrapper")) should contain (  ("p.Wrapper.Wrapper()", "p.Wrapper") : NodeIdP)

    }
  }

  scenario("generic - thread from gen return type to gen arg "){
    val _ = new ScenarioFactory(
      """package p;
        |import java.util.List;
        |class A {}
        |class B {
        | List<A> la1; List<A> la2;
        | void m(){ la1.add(la2.get(0)); }
        |} """
    ){
      graph.usesThatShouldUsesASuperTypeOf(("p.B.la2", "p.A")) should contain (("p.B.la1", "p.A") : NodeIdP)
    }
  }

  scenario("generic - type uses  constraint between type parameter and variable declaration type"){
    val sf = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T> { T get(){return null;} }
        |
        |class A{ void m(){} }
        |
        |class B {
        |    Wrapper<A> wa = new Wrapper<A>();
        |
        |    void assignA(){
        |        A a = wa.get();
        |        a.m();
        |    }
        |}""")

    import sf.{graph, idOfFullName}

    assert (graph.uses("p.B.wa", "p.Wrapper"))
    assert (graph.uses("p.B.wa", "p.A"))

    assert (graph.uses("p.B.assignA().Definition", "p.A"))
    assert (graph.uses("p.B.assignA().Definition", "p.A.m()"))

    graph.styp("p.B.wa").value should be (ParameterizedType("p.Wrapper", List(NamedType("p.A"))))

    graph.typeUsesOf(("p.B.assignA().Definition", "p.A.m()")) should contain (("p.B.assignA().Definition", "p.A") : NodeIdP)

    graph.typeMemberUsesOf(("p.B.assignA().Definition", "p.A")) should contain (("p.B.assignA().Definition", "p.A.m()") : NodeIdP)

    graph.typeMemberUsesOf(("p.B.assignA().Definition", "p.A")).size should be (1)

    graph.usesThatShouldUsesASuperTypeOf(("p.B.wa", "p.A")) should contain (("p.B.assignA().Definition", "p.A") : NodeIdP)
  }

  scenario("generic - type uses  constraint between type parameter and variable declaration type - foreach case"){
    val _ = new ScenarioFactory(
      """package p;
        |import java.util.List;
        |
        |interface I{ void m(); }
        |
        |class C {
        |    List<I> is;
        |
        |    void doAllM(){
        |        for(I i : is)
        |            i.m();
        |    }
        |}"""){



      val fieldGenTypeUse : NodeIdP = ("p.C.is", "java.util.List")
      val fieldParameterTypeUse : NodeIdP = ("p.C.is", "p.I")

      val methodTypeUse : NodeIdP = ("p.C.doAllM().Definition", "p.I")
      val methodTypeMemberUse : NodeIdP = ("p.C.doAllM().Definition", "p.I.m()")

      graph.styp("p.C.is").value should be (ParameterizedType("java.util.List", List(NamedType("p.I"))))

      graph.typeUsesOf(methodTypeMemberUse) should contain (methodTypeUse)
      graph.typeMemberUsesOf(methodTypeUse) should contain (methodTypeMemberUse)
      graph.typeMemberUsesOf(methodTypeUse).size should be (1)

      graph.usesThatShouldUsesASuperTypeOf(fieldParameterTypeUse) should contain (methodTypeUse)

    }
  }

  ignore("generic - type parameter as method parameter") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T> {  void set(T t){} }
        |
        |class A{ }
        |
        |class B {
        |    Wrapper<A> wa;
        |    void set(){ wa.set(new A()); }
        |}"""
    ) {
      val a = fullName2id("p.A")

      val wa = fullName2id("p.B.wa")

      val set = fullName2id("p.B.set().Definition")

      val genType = fullName2id("p.Wrapper")
      val genericMethod = fullName2id("p.Wrapper.set(T)")


    }
  }

  scenario("2 type variables"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |import java.util.Map;
        |import java.util.jar.Attributes;
        |
        |public class C {
        |
        |    Map<String, Attributes> datas;
        |
        |    public void m() {
        |       for(String s : datas.keySet()){
        |         Attributes i = datas.get(s);
        |       }
        |    }
        |}"""
    ){
      //      val mDef = fullName2id("p.C.m().Definition")
      //      val get = fullName2id("java.util.Map.get(Object)")

      //      val hasElement = fullName2id("p.HasElement")
      //      val obj = fullName2id("java.lang.Object")
      //      val toString_ = fullName2id("java.lang.Object.toString()")


      //      assert(graph.uses(mDef, get))
      //      assert(graph.uses(mDef, toString_))
      //
      //      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, obj))
    }
  }
}
