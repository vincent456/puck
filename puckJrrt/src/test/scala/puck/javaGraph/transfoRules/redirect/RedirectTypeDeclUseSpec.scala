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

package puck.javaGraph.transfoRules.redirect

import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.Redirection
import puck.javaGraph.ScenarioFactory
import puck.AcceptanceSpec


class RedirectTypeDeclUseSpec
  extends AcceptanceSpec {

//  scenario("find type Variable value") {
//    val _ = new ScenarioFactory(
//      """package p;
//        |
//        |class Wrapper<T> { public T get(){return null;} }
//        |
//        |class A { public void m(){} }
//        |
//        |class B {
//        |    Wrapper<A> wa = new Wrapper<A>();
//        |
//        |    void doM(){ wa.get().m(); }
//        |}""") {
//
//      println(graph.typ("p.Wrapper.get()"))
//      println(graph.kindType("p.Wrapper@T"))
//
//      def typeInContext(typeUser: NodeId, typeVariable : NodeId, ctxt : NodeId) = {
//        graph.typeUsesOf(ctxt, typeUser)
//      }
//
//
//      println(typeInContext("p.Wrapper.get()", "p.Wrapper@T", "p.B.doM().Definition"))
//      import ShowDG._
//      (graph, graph.edges).println
//
//
//
//
//    }
//  }


  scenario("From class to superType interface") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class C implements I{ public void m(){} }
        |
        |interface I { void m(); }
        |
        |class A {
        |
        |    public static void main(String[] args){
        |        A a = new A();
        |        a.mUser(new C());
        |    }
        |
        |    void mUser(C cu){ cu.m(); }
        |}"""
    ) {

      val typeUse = Uses("p.A.mUser(C).cu", "p.C")
      assert(typeUse.existsIn(graph))
      assert(Uses("p.A.mUser(C).Definition", "p.C.m()").existsIn(graph))

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          typeUse, AccessAbstraction("p.I", SupertypeAbstraction)).rvalue

      assert(Uses("p.A.mUser(C).cu", "p.I").existsIn(g2))
      assert(Uses("p.A.mUser(C).Definition", "p.I.m()").existsIn(g2))

      assert(!Uses("p.A.mUser(C).cu", "p.C").existsIn(g2))
      assert(!Uses("p.A.mUser(C).Definition", "p.C.m()").existsIn(g2))
    }
  }

  //val classToClassSupertype

  //val interfaceToInterfaceSuperType

  scenario("From class to delegator class") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Delegatee { void mUsed(){} }
        |
        |class Delegator {
        |    Delegatee d;
        |    void mUsed(){ d.mUsed(); }
        |}
        |
        |class A {
        |
        |    public static void main(String[] args){
        |        A a = new A();
        |        a.mUser(new Delegatee());
        |    }
        |
        |    void mUser(Delegatee d){ d.mUsed(); }
        |}"""
    ) {
      val mUserDecl = fullName2id("p.A.mUser(Delegatee)")
      val theParam = fullName2id("p.A.mUser(Delegatee).d")

      val mUserDef = fullName2id("p.A.mUser(Delegatee).Definition")

      val delegatee = fullName2id("p.Delegatee")
      val mDelegatee = fullName2id("p.Delegatee.mUsed()")

      val delegator = fullName2id("p.Delegator")
      val mDelegator = fullName2id("p.Delegator.mUsed()")

      val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
        .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

      val typeUse = Uses(theParam, delegatee)
      assert(typeUse.existsIn(graph))
      assert(Uses(mUserDef, mDelegatee).existsIn(graph))


      val g2 =
        Redirection.redirectUsesAndPropagate(g,
          typeUse, AccessAbstraction(delegator, DelegationAbstraction)).rvalue

      assert(Uses(theParam, delegator).existsIn(g2))
      assert(Uses(mUserDef, mDelegator).existsIn(g2))
    }

  }

  ignore("From interface to delegator class") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |interface I { void mUsed(); }
        |
        |class Impl implements I { public void mUsed(){} }
        |
        |class Delegator {
        |    I d;
        |    void mUsed(){ d.mUsed(); }
        |}
        |
        |class A {
        |
        |    public static void main(String[] args){
        |        A a = new A();
        |        a.mUser(new Impl());
        |    }
        |
        |    void mUser(I i){ i.mUsed(); }
        |}"""
    ){

    }
  }
  scenario("From class to superType interface as type parameter context") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T> {
        |    private T t;
        |    public T get(){return t;}
        |}
        |
        |interface I { void m(); }
        |
        |class A implements I { public void m(){} }
        |
        |
        |class B {
        |    Wrapper<A> wa = new Wrapper<A>();
        |
        |    void doM(){ wa.get().m(); }
        |}""") {


      import ShowDG._
      (graph, graph.edges).println

      val ltg =  Redirection.redirectUsesAndPropagate(graph,  ("p.B.wa", "p.A"),
        AccessAbstraction("p.I", SupertypeAbstraction))
      val g2 = ltg.rvalue

      import ShowDG._
      println(ltg.log)
      (g2, g2.edges).println

      assert(g2.uses("p.B.wa", "p.I"))
      assert(g2.uses("p.B.doM().Definition", "p.I.m()"))

    }
  }

  scenario("From class to superType interface - impact local variable type") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper {
        |    A a = new A();
        |    public A get(){return a;}
        |
        |}
        |
        |interface I {  }
        |
        |class A implements I {  }
        |
        |class B {
        |    Wrapper wa = new Wrapper();
        |
        |    void getA(){ A a = wa.get(); }
        |}"""
    ) {

      graph.usesThatShouldUsesASuperTypeOf(("p.Wrapper.get()", "p.A")) should contain (
        ("p.B.getA().Definition", "p.A") : NodeIdP)

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          ("p.Wrapper.get()", "p.A"), AccessAbstraction("p.I", SupertypeAbstraction)).rvalue

      assert(Uses("p.Wrapper.get()", "p.I").existsIn(g2))
      assert(Uses("p.B.getA().Definition", "p.I").existsIn(g2))

    }
  }

  scenario("From class to superType interface - type parameter context - impact local variable type") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Wrapper<T> {
        |    private T t;
        |    public void set(T t){}
        |    public T get(){return t;}
        |
        |}
        |
        |interface I { }
        |
        |class A implements I { }
        |
        |class B {
        |    Wrapper<A> wa = new Wrapper<A>();
        |
        |    void getA(){ A a = wa.get(); }
        |}"""
    ) {



      val typeUse1 : NodeIdP = ("p.B.wa", "p.A")
      val typeUse2 : NodeIdP = ("p.B.getA().Definition", "p.A")

      graph.usesThatShouldUsesASuperTypeOf(typeUse1) should contain (typeUse2)

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          typeUse1, AccessAbstraction("p.I", SupertypeAbstraction)).rvalue

      assert(Uses("p.B.wa", "p.I").existsIn(g2))
      assert(Uses("p.B.getA().Definition", "p.I").existsIn(g2))

    }
  }

}
