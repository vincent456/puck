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
import puck.graph.{DependencyGraph, ShowDG}
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 11/04/16.
  */
class ContainsSpec extends AcceptanceSpec {

  scenario("static class member") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |
        |
        |    public static class InnerA {
        |        public InnerA(){}
        |        public void innerMa(){}
        |    }
        |
        |}""") {

      val p = fullName2id("p")
      val `p.A` = fullName2id("p.A")
      val `p.A.InnerA` = fullName2id("p.A.InnerA")
      val `p.A.InnerA.InnerA()` = fullName2id("p.A.InnerA.InnerA()")
      val `p.A.InnerA.InnerA().Definition` = fullName2id("p.A.InnerA.InnerA().Definition")

      graph.container(`p.A`).value shouldBe p
      graph.content(p) should contain (`p.A`)

      graph.container(`p.A.InnerA`).value shouldBe `p.A`
      graph.content(`p.A`) should contain (`p.A.InnerA`)

      graph.container(`p.A.InnerA.InnerA()`).value shouldBe `p.A.InnerA`
      graph.content(`p.A.InnerA`) should contain (`p.A.InnerA.InnerA()`)

      graph.container(`p.A.InnerA.InnerA().Definition`).value shouldBe `p.A.InnerA.InnerA()`
      graph.content(`p.A.InnerA.InnerA()`) should contain (`p.A.InnerA.InnerA().Definition`)

    }

  }

  scenario("instance class member") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |
        |    public class InnerA {
        |        public InnerA(){}
        |        public void innerMa(){}
        |    }
        |}""") {

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
      """package p;
        |
        |interface CanDoM{ void m(); }
        |
        |public class A {
        |
        |    public static void declareInnerClass(){
        |        class CanDoMInstance implements CanDoM { public void m(){} }
        |        (new CanDoMInstance()).m();
        |    }
        |}""") {
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
      """package p;
        |class A<T>{}
        |class B<T>{}""") {

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
      """package p;
        |
        |class A<T>{
        |    <T> T castMe(Object o){ return (T)o; }
        |    <T> T castMeInstead(Object o){return (T)o;}
        |}""") {

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
    val _ = new ScenarioFactory(
      """package p;
        |
        |//interface Function<S,T>{}
        |interface Comparator<T> {
        |
        |        //real exemple comming from java.util.Comparator
        |//        <U> Comparator<T> thenComparing(Function<? super T, ? extends U> keyExtractor,
        |//                                        Comparator<? super U> keyComparator);
        |        <U> Comparator<T> thenComparing(Comparator<? super U> keyComparator);
        |}""") {
      val u = fullName2id("p.Comparator.thenComparing(Comparator)@U")
      val param = fullName2id("p.Comparator.thenComparing(Comparator).keyComparator")
      val m = fullName2id("p.Comparator.thenComparing(Comparator)")

      graph.contains(m, u)
      graph.uses(param, u)

    }
  }

  info("method m(A...) recognized as m(A[])")
  scenario("overloading with variadic method"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {
        |    void m(double d){ }
        |    void m(double... d){ }
        |}"""
    ){
      val m1name = "p.A.m(double)"
      val m1id = fullName2id(m1name)
      val m2name = "p.A.m(double[])"
      val m2id = fullName2id(m2name)

      import ShowDG._
      (graph, m1id).shows(desambiguatedFullName) shouldBe m1name
      (graph, m2id).shows(desambiguatedFullName) shouldBe m2name

    }
  }


  feature("find element by name") {
    val find = DependencyGraph.findElementByName _

    scenario("find top package") {
      val _ = new ScenarioFactory("package p;") {
        find(graph, "p").value.id shouldBe fullName2id("p")
      }
    }

    scenario("find top class") {
      val _ = new ScenarioFactory(
        """package p;
          | class A{}
        """.stripMargin) {

        find(graph, "p.A").value.id shouldBe fullName2id("p.A")
      }
    }

    scenario("find method") {
      val _ = new ScenarioFactory(
        """package p;
          | class A{ void m(){} }
        """) {

        find(graph, "p.A.m()").value.id shouldBe fullName2id("p.A.m()")
      }
    }


    scenario("synchronized block") {
      val s = new ScenarioFactory(
        """package p;
          |class A { void ma(){} }
          |
          |class B { void m(){
          |   synchronized(this){
          |     A a = new A();
          |     a.ma();
          |   }
          | }
          |}""")
      import s._
      find(graph, "p.B.m().Definition.0").value.id shouldBe fullName2id("p.B.m().Definition.0")

    }

    scenario("marauroa pb") {
      val s = new ScenarioFactory(
        """package p;
          |import java.util.Map;
          |
          |class A { void ma(){} }
          |
          |class B {
          |
          |   Map<String,String> m;
          |   void m(){
          |     synchronized(this){
          |       for(Map.Entry<String, String> val : m.entrySet()) {
          |         A a = new A();
          |         a.ma();
          |       }
          |     }
          |   }
          |}""")
      import s._
      find(graph, "p.B.m().Definition.1").value.id shouldBe fullName2id("p.B.m().Definition.1")

    }
  }
}
