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

import puck.TransfoRulesSpec
import puck.graph._
import puck.graph.constraints.DelegationAbstraction
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Redirection}
import puck.javaGraph.nodeKind.Field

/**
  * Created by Loïc Girault on 06/05/16.
  */
class RedirectTypeConstructorUsesSpec
  extends TransfoRulesSpec {

  scenario("From constructor to constructorMethod - static") {
    def code(createInstance : String) : String =
      s"""package p;
        |
        |class Factory{ static B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A { void m() { B b = $createInstance; } }"""

    compareWithExpectedAndGenerated(code("new B()"),
      bs => {
        import bs.{graph, idOfFullName}

        val g =
          graph.addAbstraction("p.B.B()", AccessAbstraction("p.Factory.createB()", DelegationAbstraction))
            .setRole("p.Factory.createB()", Some(Factory("p.B.B()")))

        Redirection.redirectUsesAndPropagate(g, ("p.A.m().Definition", "p.B.B()"),
          AccessAbstraction("p.Factory.createB()", DelegationAbstraction)).rvalue
      },
      code("Factory.createB()"))
  }

  scenario("From constructor to constructorMethod hosted elsewhere - non static, parameter") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class Factory{ B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A { void m() { B b = new B(); } }""",
      bs => {
        import bs.{graph, idOfFullName}

        val g = graph.addAbstraction("p.B.B()",
          AccessAbstraction("p.Factory.createB()", DelegationAbstraction))
          .setRole("p.Factory.createB()", Some(Factory("p.B.B()")))

        Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          ("p.A.m().Definition", "p.B.B()"): NodeIdP,
          AccessAbstraction("p.Factory.createB()", DelegationAbstraction))(CreateParameter).rvalue
      },
      """package p;
        |
        |class Factory{ B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A { void m(Factory factory) { B b = factory.createB(); } }""")
  }

  scenario("From constructor to constructorMethod hosted elsewhere - non static, field") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class Factory{ B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A { void m() { B b = new B(); } }""",
      bs => {
        import bs.{graph, idOfFullName}

        val g = graph.addAbstraction("p.B.B()",
          AccessAbstraction("p.Factory.createB()", DelegationAbstraction))
          .setRole("p.Factory.createB()", Some(Factory("p.B.B()")))

        Redirection.redirectTypeConstructorToInstanceValueDecl(g, ("p.A.m().Definition", "p.B.B()"),
          AccessAbstraction("p.Factory.createB()", DelegationAbstraction))(CreateTypeMember(Field)).rvalue

      },
      """package p;
        |
        |class Factory{ B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A {
        |   Factory factory_delegate = new Factory();
        |   void m() { B b = factory_delegate.createB(); }
        |}""")
  }

  ignore("From constructor to constructorMethod hosted by self - non static, parameter") {
    compareWithExpectedAndGenerated(
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
        |}""",
      bs => {
        import bs.{graph, idOfFullName}

        val g = graph.addAbstraction("p.B.B()", AccessAbstraction("p.B.create()", DelegationAbstraction))
          .setRole("p.B.create()", Some(Factory("p.B.B()")))

        Redirection.redirectTypeConstructorToInstanceValueDecl(g, ("p.A.m().Definition", "p.B.B()"),
          AccessAbstraction("p.B.create()", DelegationAbstraction))(CreateParameter).rvalue
      },
      """package p;
        |
        |class B { B create(){return new B();} }
        |
        |class A { void m(B b){ b.create(); } }
        |
        |class C {
        |    void mc(){
        |        A a = new A();
        |        a.m(new B());
        |    }
        |}""")
  }

  scenario("From constructor to constructorMethod hosted by self - non static, field") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class B { B create(){return new B();} }
        |
        |class A { void m(){ new B(); } }
        |
        |class C { void mc(A a){ a.m(); } }""",
      bs => {
        import bs.{graph, idOfFullName}

        val g = graph.addAbstraction("p.B.B()", AccessAbstraction("p.B.create()", DelegationAbstraction))

        Redirection.redirectTypeConstructorToInstanceValueDecl(g, ("p.A.m().Definition", "p.B.B()"),
          AccessAbstraction("p.B.create()", DelegationAbstraction))(CreateTypeMember(Field)).rvalue
      },
      """package p;
        |
        |class B { B create(){return new B();} }
        |
        |class A { B b_delegate = new B(); void m(){ b_delegate.create(); } }
        |
        |class C { void mc(A a){ a.m(); } }""")
  }

  scenario("Redirect init use from constructor to factory") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class A {
        | int f;
        | void init() { f = 42; }
        | A(){ init(); }
        | static A create(){
        |   return new A();
        | }
        |}
        """,
      bs => {
        import bs.{graph, idOfFullName}

        val g = graph.setRole("p.A.init()", Some(Initializer("p.A.f")))
                      .setRole("p.A.create()", Some(Factory("p.A.A()")))

        Redirection.redirectSourceOfInitUseInFactory(g,
          "p.A.A()", "p.A.A().Definition",
          "p.A.init()", "p.A.create()")

      },
      """package p;
        |
        |class A {
        | int f;
        | void init() { f = 42; }
        | A(){}
        | static A create(){
        |   A ret = new A();
        |   ret.init();
        |   return ret;
        | }
        |}
      """)
  }

}
