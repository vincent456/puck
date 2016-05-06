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

import puck.graph.constraints.DelegationAbstraction
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Redirection}
import puck.graph.{AccessAbstraction, Uses}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Field
import puck.AcceptanceSpec

/**
  * Created by Loïc Girault on 24/03/16.
  */
class RedirectTypeConstructorUsesSpec
  extends AcceptanceSpec {

  scenario("From constructor to constructorMethod hosted elsewhere - non static, parameter") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Factory{ B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A { void m() { B b = new B(); } }"""
    ) {



      val ctorUse = Uses("p.A.m().Definition", "p.B.B()")
      val ctorMethodUse = Uses("p.A.m().Definition", "p.Factory.createB()")

      assert(ctorUse.existsIn(graph))
      assert(!ctorMethodUse.existsIn(graph))

      graph.parametersOf("p.A.m()") shouldBe empty

      val g = graph.addAbstraction("p.B.B()", AccessAbstraction("p.Factory.createB()", DelegationAbstraction))

      val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
        ctorUse, AccessAbstraction("p.Factory.createB()", DelegationAbstraction))(CreateParameter).rvalue

      assert(ctorMethodUse.existsIn(g2))
      assert(!ctorUse.existsIn(g2))

      val parameters = g2.parametersOf("p.A.m()")
      parameters.size shouldBe 1

      assert(g2.uses(parameters.head, "p.Factory"))
    }
  }

  scenario("From constructor to constructorMethod hosted elsewhere - non static, field") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class Factory{ B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A { void m() { B b = new B(); } }"""
    ) {

      val ctorUse = Uses("p.A.m().Definition", "p.B.B()")
      val ctorMethodUse = Uses("p.A.m().Definition", "p.Factory.createB()")

      assert(ctorUse.existsIn(graph))
      assert(!ctorMethodUse.existsIn(graph))

      val g = graph.addAbstraction("p.B.B()", AccessAbstraction("p.Factory.createB()", DelegationAbstraction))

      val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
        ctorUse, AccessAbstraction("p.Factory.createB()", DelegationAbstraction))(CreateTypeMember(Field)).rvalue

      assert(ctorMethodUse.existsIn(g2))
      assert(!ctorUse.existsIn(g2))

      g2.content("p.A").size shouldBe (graph.content("p.A").size + 1)

      //assert(g2.uses(parameters.head, factoryClass))
    }
  }


  scenario("From constructor to constructorMethod hosted by self - non static, parameter") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class B { B create(){return new B();} }
        |
        |class A { void m(){ new B(); } }
        |
        |class C {
        |    void mc(){
        |        A a = new A();
        |        a.m();
        |    }
        |}"""
    ) {



      val ctorUse = Uses("p.A.m().Definition", "p.B.B()")
      val ctorMethodUse = Uses("p.A.m().Definition", "p.B.create()")

      assert(ctorUse existsIn graph)
      assert(!(ctorMethodUse existsIn graph))

      graph.parametersOf("p.A.m()") shouldBe empty

      val g = graph.addAbstraction("p.B.B()", AccessAbstraction("p.B.create()", DelegationAbstraction))

      val g2 =
        Redirection.redirectTypeConstructorToInstanceValueDecl(g, ctorUse,
          AccessAbstraction("p.B.create()", DelegationAbstraction))(CreateParameter).rvalue


      assert(!(ctorUse existsIn g2))
      assert(ctorMethodUse existsIn g2)

      val parameters = g2.parametersOf("p.A.m()")
      parameters.size shouldBe 1

      assert(g2.uses(parameters.head, "p.B"))

      assert(g2.uses("p.C.mc().Definition", "p.B.B()"))
      assert(!g2.uses("p.C.mc().Definition", "p.B"))

    }
  }

  scenario("From constructor to constructorMethod hosted by self - non static, field") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class B { B create(){ return new B(); } }
        |
        |class A { void m(){ new B(); } }
        |
        |class C {
        |    void mc(){
        |        A a = new A();
        |        a.m();
        |    }
        |}"""
    ) {

      val ctorUse = Uses("p.A.m().Definition", "p.B.B()")
      val ctorMethodUse = Uses("p.A.m().Definition", "p.B.create()")

      val callerHostClass = fullName2id("p.A")

      assert(ctorUse existsIn graph)
      assert(!(ctorMethodUse existsIn graph))


      val g = graph.addAbstraction("p.B.B()", AccessAbstraction("p.B.create()", DelegationAbstraction))

      val g2 =
        Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          ctorUse, AccessAbstraction("p.B.create()", DelegationAbstraction))(CreateTypeMember(Field)).rvalue


      assert(!(ctorUse existsIn g2))
      assert(ctorMethodUse existsIn g2)

      g2.content(callerHostClass).size shouldBe (graph.content(callerHostClass).size + 1)

      val delegate = g2.nodes.find(n => n.name(g2).endsWith("_delegate")).value

      assert(g2.uses(g2.definitionOf_!(delegate.id), "p.B.B()"))
      assert(!g2.uses("p.C.mc().Definition", "p.B"))

    }
  }

}
