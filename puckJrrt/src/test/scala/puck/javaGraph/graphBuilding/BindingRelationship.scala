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
import puck.graph.Uses
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 11/04/16.
  */
class BindingRelationship extends AcceptanceSpec {

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
    val _ = new ScenarioFactory(
      """package p;
        |
        |class B { public void mb(){} }
        |
        |class A { public void ma(B b){ b.mb(); } }"""
    ){

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
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A { void m(){ System.out.println("A.m()"); } }
        |class Cond {
        |    void condM(boolean c, A a) {  (c ? new A() : a).m(); }
        |}"""){

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
