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

package puck.javaGraph.commutativity.redirect

import puck.Settings.outDir
import puck.TransfoRulesSpec
import puck.graph.comparison.Mapping
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Redirection}
import puck.graph.{AccessAbstraction, Factory, NodeIdP, Uses}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Field

class RedirectTypeDeclUsesSpec
  extends TransfoRulesSpec {

  scenario("From class to interface superType") {
    compareWithExpectedAndGenerated(
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
        |        a.user(new C());
        |    }
        |
        |    void user(C c){ c.m(); }
        |}""",
      bs => {
        import bs.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph,
          ("p.A.user(C).c", "p.C"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      },
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
        |        a.user(new C());
        |    }
        |
        |    void user(I c){ c.m(); }
        |}"""
    )
  }

  ignore("From class to class superType"){}

  ignore("From interface to interface superType"){}

  ignore("From class to delegator class") {
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
      val theParam = fullName2id("p.A.mUser(Delegatee).d")

      val delegatee = fullName2id("p.Delegatee")
      val mDelegatee = fullName2id("p.Delegatee.mUsed()")

      val delegator = fullName2id("p.Delegator")
      val mDelegator = fullName2id("p.Delegator.mUsed()")

      //QuickFrame(graph, "g", JavaDotHelper)

      val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
        .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

      val g2 =
        Redirection.redirectUsesAndPropagate(g,
          Uses(theParam, delegatee),
          AccessAbstraction(delegator, DelegationAbstraction)).rvalue

      val recompiledEx = applyChangeAndMakeExample(g2, outDir)

      assert(Mapping.equals(g2, recompiledEx.graph))
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



}