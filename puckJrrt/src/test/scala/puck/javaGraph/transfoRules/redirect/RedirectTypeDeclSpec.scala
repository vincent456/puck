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
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Redirection}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind._
import puck.{AcceptanceSpec, Settings}

//import scalaz.syntax.show._
//import puck.util.Debug.showNodeIndex
//println(graph.nodesIndex.shows)




class RedirectTypeDeclSpec
  extends AcceptanceSpec {

  val examplesPath = s"${Settings.testExamplesPath}/redirection/typeDecl/"

  scenario("From class to superType interface") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class ClassUsed implements SuperType{ public void mUsed(){} }
        |
        |interface SuperType { void mUsed(); }
        |
        |class A {
        |
        |    public static void main(String[] args){
        |        A a = new A();
        |        a.mUser(new ClassUsed());
        |    }
        |
        |    void mUser(ClassUsed cu){ cu.mUsed(); }
        |}"""
    ) {
      val mUserDecl = fullName2id("p.A.mUser(ClassUsed)")
      val theParam = fullName2id("p.A.mUser(ClassUsed).cu")
      val mUserDef = fullName2id("p.A.mUser(ClassUsed).Definition")

      val classUsed = fullName2id("p.ClassUsed")
      val mUsed = fullName2id("p.ClassUsed.mUsed()")
      val superType = fullName2id("p.SuperType")
      val absmUsed = fullName2id("p.SuperType.mUsed()")

      val typeUse = Uses(theParam, classUsed)
      assert(typeUse.existsIn(graph))
      assert(Uses(mUserDef, mUsed).existsIn(graph))

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          typeUse, AccessAbstraction(superType, SupertypeAbstraction)).rvalue

      assert(Uses(theParam, superType).existsIn(g2))
      assert(Uses(mUserDef, absmUsed).existsIn(g2))

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
        |    public void set(T t){}
        |    public T get(){return t;}
        |
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
      val actualTypeParam = fullName2id("p.A")
      val actualTypeParamMethod = fullName2id("p.A.m()")

      val interfaceTypeParam = fullName2id("p.I")
      val interfaceMethod = fullName2id("p.I.m()")

      val field = fullName2id("p.B.wa")

      val userClass = fullName2id("p.B")
      val userMethodDef = fullName2id("p.B.doM().Definition")

      val genType = fullName2id("p.Wrapper")
      val genericMethod = fullName2id("p.Wrapper.get()")

      val fieldGenTypeUse = graph.getUsesEdge(field, genType).value
      val fieldParameterTypeUse = graph.getUsesEdge(field, actualTypeParam).value
      val typeMemberUse = graph.getUsesEdge(userMethodDef, actualTypeParamMethod).value

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          fieldParameterTypeUse, AccessAbstraction(interfaceTypeParam, SupertypeAbstraction)).rvalue

      assert(Uses(field, interfaceTypeParam).existsIn(g2))
      assert(Uses(userMethodDef, interfaceMethod).existsIn(g2))

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
      val typeUsed = fullName2id("p.A")

      val superType = fullName2id("p.I")

      val typeUser1 = fullName2id("p.Wrapper.get()")
      val typeUser2 = fullName2id("p.B.getA().Definition")

      val typeUse1 = graph.getUsesEdge(typeUser1, typeUsed).value
      val typeUse2 = graph.getUsesEdge(typeUser2, typeUsed).value

      graph.usesThatShouldUsesASuperTypeOf(typeUse1) should contain (typeUse2)

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          typeUse1, AccessAbstraction(superType, SupertypeAbstraction)).rvalue

      assert(Uses(typeUser1, superType).existsIn(g2))
      assert(Uses(typeUser2, superType).existsIn(g2))

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
        |
        |class B {
        |    Wrapper<A> wa = new Wrapper<A>();
        |
        |    void getA(){ A a = wa.get(); }
        |}"""
    ) {
      val typeUsed = fullName2id("p.A")

      val superType = fullName2id("p.I")

      val typeUser1 = fullName2id("p.B.wa")
      val typeUser2 = fullName2id("p.B.getA().Definition")

      val typeUse1 = graph.getUsesEdge(typeUser1, typeUsed).value
      val typeUse2 = graph.getUsesEdge(typeUser2, typeUsed).value

      graph.usesThatShouldUsesASuperTypeOf(typeUse1) should contain (typeUse2)

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          typeUse1, AccessAbstraction(superType, SupertypeAbstraction)).rvalue

      assert(Uses(typeUser1, superType).existsIn(g2))
      assert(Uses(typeUser2, superType).existsIn(g2))

    }
  }

}
