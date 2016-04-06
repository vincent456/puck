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
import puck.graph.{NamedType, ParameterizedType, Uses}
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 06/04/16.
  */
class TypeBindingSpec extends AcceptanceSpec {

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

      val actualTypeParam = fullName2id("p.A")
      val actualTypeParamMethod = fullName2id("p.A.m()")

      val field = fullName2id("p.B.wa")

      val userMethodDef = fullName2id("p.B.doM().Definition")

      val genType = fullName2id("p.Wrapper")
      val genericMethod = fullName2id("p.Wrapper.get()")

      val fieldGenTypeUse = graph.getUsesEdge(field, genType).value
      val fieldParameterTypeUse = graph.getUsesEdge(field, actualTypeParam).value
      val typeMemberUse = graph.getUsesEdge(userMethodDef, actualTypeParamMethod).value
      val genericMethodUse = graph.getUsesEdge(userMethodDef, genericMethod).value

      graph.styp(field).value should be (ParameterizedType(genType, List(NamedType(actualTypeParam))))

      //typeMemberUse = ("p.B.doMonA().Definition", "p.A.m()")
      //fieldGenTypeUse = ("p.B.wa", "p.Wrapper")
      //fieldParameterTypeUse = ("p.B.wa", "p.A")
      //graph.typeUsesOf(typeMemberUse) should contain (fieldGenTypeUse)
      graph.typeUsesOf(typeMemberUse) should contain (fieldParameterTypeUse)
      graph.typeMemberUsesOf(fieldParameterTypeUse) should contain (typeMemberUse)
      graph.typeMemberUsesOf(fieldParameterTypeUse).size should be (1)
      //graph.typeMemberUsesOf(fieldParameterTypeUse) should not contain (genericMethodUse)
    }


  }

  scenario("generic - type uses  relationship between type parameter and variable declaration type"){
    val _ = new ScenarioFactory(
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
        |}"""){

      val actualTypeParam = fullName2id("p.A")
      val actualTypeParamMethod = fullName2id("p.A.m()")

      val field = fullName2id("p.B.wa")
      val userMethodDef = fullName2id("p.B.assignA().Definition")
      val genType = fullName2id("p.Wrapper")

      val fieldGenTypeUse = graph.getUsesEdge(field, genType).value
      val fieldParameterTypeUse = graph.getUsesEdge(field, actualTypeParam).value

      val methodTypeUse = graph.getUsesEdge(userMethodDef, actualTypeParam).value
      val methodTypeMemberUse = graph.getUsesEdge(userMethodDef, actualTypeParamMethod).value

      graph.styp(field).value should be (ParameterizedType(genType, List(NamedType(actualTypeParam))))

      graph.typeUsesOf(methodTypeMemberUse) should contain (methodTypeUse)
      graph.typeMemberUsesOf(methodTypeUse) should contain (methodTypeMemberUse)
      graph.typeMemberUsesOf(methodTypeUse).size should be (1)

      graph.usesThatShouldUsesASuperTypeOf(fieldParameterTypeUse) should contain (methodTypeUse)


    }
  }

  scenario("generic - type uses  relationship between type parameter and variable declaration type - foreach case"){
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

      val actualTypeParam = fullName2id("p.I")
      val actualTypeParamMethod = fullName2id("p.I.m()")

      val field = fullName2id("p.C.is")
      val userMethodDef = fullName2id("p.C.doAllM().Definition")
      val genType = fullName2id("java.util.List")

      val fieldGenTypeUse = graph.getUsesEdge(field, genType).value
      val fieldParameterTypeUse = graph.getUsesEdge(field, actualTypeParam).value

      val methodTypeUse = graph.getUsesEdge(userMethodDef, actualTypeParam).value
      val methodTypeMemberUse = graph.getUsesEdge(userMethodDef, actualTypeParamMethod).value

      graph.styp(field).value should be (ParameterizedType(genType, List(NamedType(actualTypeParam))))

      graph.typeUsesOf(methodTypeMemberUse) should contain (methodTypeUse)
      graph.typeMemberUsesOf(methodTypeUse) should contain (methodTypeMemberUse)
      graph.typeMemberUsesOf(methodTypeUse).size should be (1)

      graph.usesThatShouldUsesASuperTypeOf(fieldParameterTypeUse) should contain (methodTypeUse)

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

      val typeValue = fullName2id("p.C")
      val typeValueMethod = fullName2id("p.C.execute()")

      val userMethodDef = fullName2id("p.WC.m().Definition")
      val genType = fullName2id("p.Wrapper")
      val wc = fullName2id("p.WC")


      val classTypeUse = graph.getUsesEdge(wc, typeValue).value
      val useOfExecute = graph.getUsesEdge(userMethodDef, typeValueMethod).value


      assert( classTypeUse existsIn graph )
      assert( useOfExecute existsIn graph )

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

      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, string))
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

      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, obj))
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

      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, string))
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
      val mDef = fullName2id("p.C.m().Definition")
      val getE = fullName2id("p.ListModel.getE()")

      val hasElement = fullName2id("p.HasElement")
      val obj = fullName2id("java.lang.Object")
      val toString_ = fullName2id("java.lang.Object.toString()")


      assert(graph.uses(mDef, getE))
      assert(graph.uses(mDef, toString_))

      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, obj))
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
  scenario("Use of boo"){
    val _ = new ScenarioFactory(
/*      """package p;
        |
        |class Wrapper<T> {  T get(){return null;} }
        |class WO extends Wrapper<Object>{}
        |class WWO extends Wrapper<WO> {}
        |public class C {
        |
        |    WWO wwo;
        |
        |    public void m() {
        |       WO wo = wwo.get();
        |       Object o = wo.get();
        |    }
        |}"""*/
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
