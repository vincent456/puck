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
import puck.graph.{AccessAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.Redirection

/**
  * Created by Loïc Girault on 06/05/16.
  */
class RedirectMethodUsesSpec
  extends TransfoRulesSpec {

  info("From method to abstract method in superType")

  scenario("methods without arguments"){
    def code(tName : String) : String =
      s"""package p;
          |
          |interface B {
          |    void m1();
          |    void m2();
          |}
          |
          |class Bimpl implements B {
          |    public void m1(){}
          |    public void m2(){}
          |}
          |
          |class A {
          |    void m(){
          |        $tName b = new Bimpl();
          |        b.m1();
          |        b.m2();
          |    }
          |}"""

    compareWithExpectedAndGenerated( code("Bimpl"),
      bs => {
        import bs.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph, ("p.A.m().Definition", "p.Bimpl.m1()"),
          AccessAbstraction("p.B.m1()", SupertypeAbstraction)).rvalue
      },
      code("B"))

  }

  scenario("Parameterized type T<X> and argument typed as X - short version"){
    def code(tName : String) =
      s"""package fileSystem;
          |
          |import java.util.ArrayList;
          |import java.util.List;
          |
          |interface FSElement { }
          |
          |public class Directory implements FSElement {
          |   public void add( $tName d ) { directories.add( d ); }
          |//   private List<FSElement> temoin;
          |   private List<$tName> directories = new ArrayList<$tName>();
          |}"""

    compareWithExpectedAndGenerated( code("Directory"),
      s => {
        import s.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph, ("fileSystem.Directory.add(Directory).d", "fileSystem.Directory"),
          AccessAbstraction("fileSystem.FSElement", SupertypeAbstraction)).rvalue
      },
      code("FSElement"))
  }

  scenario("Parameterized type T<X> and argument typed as X"){
    def code(tName : String) =
      s"""package fileSystem;
          |
          |import java.util.ArrayList;
          |import java.util.List;
          |
          |interface FSElement { void display(String path); }
          |
          |public class Directory implements FSElement {
          |   public Directory( String name ) { this.name = name; }
          |   public void add( $tName d ) { directories.add( d ); }
          |   public void display(String path) {
          |      System.out.println(path + name);
          |      String npath = path + name +"/";
          |      for($tName d: directories)
          |         d.display(npath);
          |   }
          |   private String    name;
          |
          |   private List<$tName> directories = new ArrayList<$tName>();
          |}"""

    compareWithExpectedAndGenerated( code("Directory"),
      s => {
        import s.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph,
          ("fileSystem.Directory.add(Directory).d", "fileSystem.Directory"),
          AccessAbstraction("fileSystem.FSElement", SupertypeAbstraction)).rvalue
      },
      code("FSElement"))
  }


  def genCode(hasAgetA : String, usesA : String, cGetA : String) : String =
    s"""package p;
      |
      |interface I {} class A implements I {}
      |interface HasA { $hasAgetA getA(); }
      |interface UseA { void usesA($usesA a);}
      |
      |class C {
      |   HasA delegate;
      |   $cGetA getA(){ return delegate.getA();  }
      |   void doStuff(UseA u){ u.usesA(delegate.getA());}
      |}
      |
    """

  scenario("test 1"){
    compareWithExpectedAndGenerated(genCode("A", "A", "A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.C.getA()", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, genCode("A", "A", "I"))
  }

  scenario("test 2"){
    compareWithExpectedAndGenerated(genCode("A", "A", "A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.UseA.usesA(A).a", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, genCode("A", "I", "A"))
  }

  scenario("test 3"){
    compareWithExpectedAndGenerated(genCode("A", "A", "A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.HasA.getA()", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, genCode("I", "I", "I"))
  }

  def genCodeParamType(hasAgetA : String, usesA : String, cGetA : String) : String =
    s"""package p;
        |
        |interface I {} class A implements I {}
        |interface Has<T> { T get(); }
        |interface UseA { void usesA($usesA a);}
        |
        |class C {
        |   Has<$hasAgetA> delegate;
        |   $cGetA getA(){ return delegate.get();  }
        |   void doStuff(UseA u){ u.usesA(delegate.get());}
        |}
        |
    """

  scenario("param type test 1"){
    compareWithExpectedAndGenerated(genCodeParamType("A", "A", "A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.C.getA()", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, genCodeParamType("A", "A", "I"))
  }

  scenario("param type test 2"){
    compareWithExpectedAndGenerated(genCodeParamType("A", "A", "A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.UseA.usesA(A).a", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, genCodeParamType("A", "I", "A"))
  }

  scenario("param type test 3"){
    compareWithExpectedAndGenerated(genCodeParamType("A", "A", "A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.C.delegate", "p.A"),
          AccessAbstraction("p.I", SupertypeAbstraction)).rvalue
      }, genCodeParamType("I", "I", "I"))
  }


  def genCodeParamTypeMultiUsage(ca : String, da : String) =
    s"""package p;
      |interface I { void ma(); }
      |class A implements I{ public void ma(){} }
      |class Gen<T> { T t; T get(){ return t; } }
      |class C { Gen<$ca> g; void mc(){ g.get().ma(); } }
      |class D { Gen<$da> g; void md(){ g.get().ma(); } }"""

  scenario("param type multi usage test 1"){
    compareWithExpectedAndGenerated(genCodeParamTypeMultiUsage("A", "A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.C.mc().Definition", "p.A.ma()"),
          AccessAbstraction("p.I.ma()", SupertypeAbstraction)).rvalue
      }, genCodeParamTypeMultiUsage("I", "A"))
  }
  def genCodeParamTypeMultiUsageSameClass(ca1 : String) =
    s"""package p;
        |interface I { void ma(); }
        |class A implements I{ public void ma(){} }
        |class B { public void mb(){} }
        |class Gen<T> { T t; T get(){ return t; } }
        |class C { Gen<$ca1> g1; Gen<B> g2;
        |     void mc(){ g1.get().ma();  g2.get().mb();}
        |}"""

  scenario("param type multi usage in same method"){
    compareWithExpectedAndGenerated(genCodeParamTypeMultiUsageSameClass("A"),
      s => {
        import s.{graph, idOfFullName}
        Redirection.redirectUsesAndPropagate(graph, ("p.C.mc().Definition", "p.A.ma()"),
          AccessAbstraction("p.I.ma()", SupertypeAbstraction)).rvalue
      }, genCodeParamTypeMultiUsageSameClass("I"))
  }

  ignore("From method to method delegate"){

  }



}
