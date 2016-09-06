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
import puck.graph.transformations.rules.Redirection
import puck.graph.{AccessAbstraction, DelegationAbstraction, SupertypeAbstraction}
import puck.javaGraph.ScenarioFactory

class RedirectTypeDeclUsesSpec
  extends TransfoRulesSpec {


  scenario("From class to interface superType") {
    def code(typeUsed : String) =
      s"""package p;
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
        |    void user($typeUsed c){ c.m(); }
        |}"""

    compareWithExpectedAndGenerated(
      code("C"),
      bs => {
        import bs.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph,
          ("p.A.user(C).c", "p.C"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, code("I"))
  }

  scenario("From class to interface superType - with method with parameters") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class C implements I{ public void m(int i){} }
        |
        |interface I { void m(int i); }
        |
        |class A { int f = 42; void user(C c){ c.m(f); } }""",
      bs => {
        import bs.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph, ("p.A.user(C).c", "p.C"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      },
      """package p;
        |
        |class C implements I{ public void m(int i){} }
        |
        |interface I { void m(int i); }
        |
        |class A { int f = 42;  void user(I c){ c.m(f); } }"""
    )
  }

  scenario("From class to interface superType - type parameter propagation") {
    def code(lType : String ) =
      s"""package p;
        |
        |import java.util.ArrayList;
        |import java.util.List;
        |
        |interface Vehicule {}
        |
        |class Train implements Vehicule {}
        |
        |public class ClientTrain {
        |    List<$lType> trains = new ArrayList<$lType>();
        |    List<Integer> horaires = new ArrayList<Integer>();
        |
        |    public List<$lType> nombreTrainsAvant(int h) {
        |        List<$lType> avant = new ArrayList<$lType>();
        |        for(int i =0; i < trains.size(); i++){
        |            if(horaires.get(i) < h)
        |                avant.add(trains.get(i));
        |        }
        |
        |        return avant;
        |    }
        |
        |}"""

    compareWithExpectedAndGenerated(
      code("Train"),

      bs => {
        import bs.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph, ("p.ClientTrain.trains", "p.Train"),
          AccessAbstraction("p.Vehicule", SupertypeAbstraction)).rvalue
      },

      code("Vehicule"))
  }


  scenario("From class to interface superType - return type propagation") {
    def code(typeUsed : String ) =
      s"""package p;
        |
        |class C implements I{ public void m(int i){} }
        |
        |interface I { void m(int i); }
        |
        |abstract class A { abstract $typeUsed getC(); void user(){ getC().m(42); } }"""

    compareWithExpectedAndGenerated(code("C"),
      bs => {
        import bs.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.A.user().Definition", "p.C.m(int)"),
          AccessAbstraction("p.I.m(int)", SupertypeAbstraction)).rvalue
      }, code("I"))
  }

  scenario("From class to interface superType - return type propagation, chained call") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class C implements I{ public void m(int i){} }
        |
        |interface I { void m(int i); }
        |
        |abstract class A { abstract C getC(); }
        |abstract class B { abstract A getA(); void user(){ getA().getC().m(42); } }
        |""",
      bs => {
        import bs.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.B.user().Definition", "p.C.m(int)"),
          AccessAbstraction("p.I.m(int)", SupertypeAbstraction)).rvalue
      },
      """package p;
        |
        |class C implements I{ public void m(int i){} }
        |
        |interface I { void m(int i); }
        |
        |abstract class A { abstract I getC(); }
        |abstract class B { abstract A getA(); void user(){ getA().getC().m(42); } }"""
    )
  }


  scenario("From class to interface superType - return type propagation, impact local var type") {
    def code(typeUsed : String ) =
      s"""package p;
        |
        |class C implements I{ }
        |
        |interface I {  }
        |
        |abstract class A { abstract $typeUsed getC(); void user(){ $typeUsed c = getC(); } }"""
    compareWithExpectedAndGenerated(code("C"),
      bs => {
        import bs.{graph, idOfFullName}
        bs.printFullNamesSortedByKey
        Redirection.redirectUsesAndPropagate(graph, ("p.A.getC()", "p.C"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      },code("I"))
  }

  scenario("From class to interface superType - used in type parameter context") {
    def code(typeUsed : String ) =
      s"""package p;
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
        |class B {
        |    Wrapper<$typeUsed> wa = new Wrapper<$typeUsed>();
        |
        |    void doM(){ wa.get().m(); }
        |}"""

    compareWithExpectedAndGenerated(code("A"),
      bs => {
        import bs.{graph, idOfFullName}
        val g2 = Redirection.redirectUsesAndPropagate(graph, ("p.B.wa", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue

        assert(g2.uses("p.B.doM().Definition", "p.I.m()"))

        g2
      },code("I"))
  }

  scenario("From class to interface superType - used in type parameter context, impact local var type") {
    def code(typeUsed : String ) =
      s"""package p;
          |
          |class Wrapper<T> {
          |    private T t;
          |    public T get(){return t;}
          |}
          |
          |interface I {  }
          |
          |class A implements I { }
          |
          |
          |class B {
          |    Wrapper<$typeUsed> wa = new Wrapper<$typeUsed>();
          |
          |    void doM(){ $typeUsed a = wa.get(); }
          |}"""

    compareWithExpectedAndGenerated(code("A"),
      bs => {
        import bs.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.B.wa", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, code("I"))
  }

  scenario("From class to interface superType - used in wildcard context"){
    def code(typeUsed : String ) =
      s"""package p;
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
          |class B {
          |    Wrapper<? extends $typeUsed> wa = new Wrapper<$typeUsed>();
          |
          |    void doM(){ wa.get().m(); }
          |}"""

    compareWithExpectedAndGenerated(code("A"),
      bs => {
        import bs.{graph, idOfFullName}
        val g2 = Redirection.redirectUsesAndPropagate(graph, ("p.B.wa", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue

        assert(g2.uses("p.B.doM().Definition", "p.I.m()"))

        g2
      },code("I"))
  }

  ignore("From class to class superType"){}

  ignore("From interface to interface superType"){}

  ignore("From class to delegator class") {

    def code(typeUsed : String) =
      s"""package p;
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
        |    void mUser($typeUsed d){ d.mUsed(); }
        |}"""

    compareWithExpectedAndGenerated(
      code("Delegatee"),
      bs => {
        import bs.{graph, idOfFullName}
        val g = graph.addAbstraction("p.Delegatee", AccessAbstraction("p.Delegator", DelegationAbstraction))
          .addAbstraction("p.Delegatee.mUsed()", AccessAbstraction("p.Delegator.mUsed()", DelegationAbstraction))

        Redirection.redirectUsesAndPropagate(g, ("p.A.mUser(Delegatee).d", "p.Delegatee"),
            AccessAbstraction("p.Delegator", DelegationAbstraction)).rvalue
      },
      code("Delegator"))
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
