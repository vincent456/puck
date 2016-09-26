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
import puck.javaGraph.ScenarioFactory
import puck.graph._
/**
  * Created by Loïc Girault on 06/04/16.
  */
class TypeConstraintSpec extends AcceptanceSpec {

  //  scenario("Field init") {
  //    val _ = new ScenarioFactory(
  //      """package p;
  //        |
  //        |class A {}
  //        |
  //        |class B {  A a = new A(); } """
  //    ) {
  //      graph.typeConstraints("p.B.a") should contain (Sub(TypeOf("p.A.A()"), TypeOf("p.B.a")))
  //      assert( graph.typeConstraints("p.B.a") forall (TypeConstraint.comply(graph, _)) )
  //    }
  //  }
  //
  //  scenario("Field assignment") {
  //    val _ = new ScenarioFactory(
  //      """package p;
  //        |
  //        |class A {  }
  //        |
  //        |class B {  A a;  void m(){ a = new A(); }  } """
  //    ) {
  //      graph.typeConstraints("p.B.a") should contain (Sub(TypeOf("p.A.A()"), TypeOf("p.B.a")))
  //      assert( graph.typeConstraints("p.B.a") forall (TypeConstraint.comply(graph, _)) )
  //    }
  //  }

  ignore("generic - type uses  FieldDecl constraint "){
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

      //      graph.nodesThatShouldHaveSameTypeAs("p.B.wa") should contain ( ??? : NodeId )
      //      graph.nodesThatShouldBeASupertypeOf( ??? ) should contain ( "p.B.wa" : NodeId )
      //      graph.nodesThatShouldBeASubtypeOf("p.B.wa") should contain (  ??? : NodeId)

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
    val s = new ScenarioFactory(
      """package p;
        |import java.util.List;
        |class A {}
        |class B {
        | List<A> la1; List<A> la2;
        | void m(){ la1.add(la2.get(0)); }
        |} """
    )
    import s.{graph, idOfFullName}

    graph.typeConstraints("p.B.la2") should contain (Sub(ParTypeProjection(TypeOf("p.B.la2"), 0),
      ParTypeProjection(TypeOf("p.B.la1"), 0)))
    assert( graph.typeConstraints("p.B.la2") forall (TypeConstraint.comply(graph, _)) )
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

    //assert (graph.uses("p.B.assignA().Definition", "p.A"))
    assert (graph.uses("p.B.assignA().Definition", "p.A.m()"))

    graph.styp("p.B.wa").value should be (ParameterizedType("p.Wrapper", List(NamedType("p.A"))))

    graph.typeConstraints("p.B.assignA().Definition.0") should contain (Sub(ParTypeProjection(TypeOf("p.B.wa"),0),
      TypeOf("p.B.assignA().Definition.0")))

  }

  scenario("param type subtyping"){
    val s = new ScenarioFactory(
      """package p;
        |import java.util.List;
        |
        |interface I{ }
        |
        |class C {
        |    List<I> is;
        |    I i;
        |}""")
    import s._

    val `list<I>Type` = ParameterizedType("java.util.List", List(NamedType("p.I")))
    val `iterable<I>Type` = ParameterizedType("java.lang.Iterable", List(NamedType("p.I")))

    graph.styp("p.C.is").value should be (`list<I>Type`)
    assert(graph.isa_*("java.util.List", "java.lang.Iterable"))
    assert (`list<I>Type`.subtypeOf(graph,`iterable<I>Type`))
    assert (`list<I>Type`.subtypeOf(graph,`list<I>Type`))
    assert (TypeConstraint.comply(graph, Sub(TypeOf("p.C.i"),
      ParTypeProjection(TypeOf("p.C.is"), 0))))
    //      graph.typeUsesOf(methodTypeMemberUse) should contain (methodTypeUse)
    //      graph.typeMemberUsesOf(methodTypeUse) should contain (methodTypeMemberUse)
    //      graph.typeMemberUsesOf(methodTypeUse).size should be (1)

  }

  ignore("generic - type uses  constraint between type parameter and variable declaration type - foreach case"){
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

      val methodTypeUse : NodeIdP = ("p.C.doAllM().Definition", "p.I")
      val methodTypeMemberUse : NodeIdP = ("p.C.doAllM().Definition", "p.I.m()")

      graph.styp("p.C.is").value should be (ParameterizedType("java.util.List", List(NamedType("p.I"))))

      graph.typeUsesOf(methodTypeMemberUse) should contain (methodTypeUse)
      graph.typeMemberUsesOf(methodTypeUse) should contain (methodTypeMemberUse)
      graph.typeMemberUsesOf(methodTypeUse).size should be (1)

      ???

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

  ignore("2 type variables"){
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

  scenario("2 types param"){
    val s = new ScenarioFactory(
      """package p;
        |
        |import java.util.Map;
        |
        |public class C {
        |
        |    public void m(Map<String, String> datas) {
        |       String value = datas.get("key");
        |    }
        |}"""
      )
      import s._
//    graph.typeConstraints("p.C.m(Map).Definition.0") foreach {
//      tc =>
//        import ShowDG._
//        (graph, tc).println
//    }

    graph.typeConstraints("p.C.m(Map).Definition.0") should contain (
      BinaryTypeConstraint(Sub, ParTypeProjection(TypeOf("p.C.m(Map).datas"),1), TypeOf("p.C.m(Map).Definition.0"))
    )

  }
}
