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

package puck.javaGraph.commutativity


import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.javaGraph.ScenarioFactory
import puck.jastadd.ExtendJGraphUtils.Rules
import puck.javaGraph.nodeKind.{Interface, StaticMethod}
import puck.TransfoRulesSpec
import puck.Settings.outDir
class AbstractSpec extends TransfoRulesSpec {


  feature("Abstract class into interface") {

    info("no pre-existing super type")

    scenario("simple case - method without args") {

      compareWithExpectedAndGenerated(
        """package p;
          |class A {
          |    private int f;
          |    public void m(){}
          |}""",
        bs => {
          import bs.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)

        },
        """package p;
          |interface A_SupertypeAbstraction { void m(); }
          |class A implements A_SupertypeAbstraction {
          |    private int f;
          |    public void m(){}
          |}""")

    }

    scenario("simple case - method with one arg"){
      compareWithExpectedAndGenerated(
        """package p;
          |class B {}
          |class A { public void m(B b){} }""",
        bs => {
          import bs.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
            Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
      },
        """package p;
          |class B {}
          |interface A_SupertypeAbstraction { public void m(B b); }
          |class A implements A_SupertypeAbstraction { public void m(B b){} } """)

    }

    scenario("method self use in class"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |    public void m(){}
          |    public void methodUser(A a){ a.m(); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
            Interface, SupertypeAbstraction).rvalue

        g0.addContains("p", itc)
      },
        """package p;
          |
          |interface A_SupertypeAbstraction {
          |    void m();
          |    void methodUser(A_SupertypeAbstraction a);
          |}
          |class A implements A_SupertypeAbstraction{
          |    public void m(){}
          |    public void methodUser(A_SupertypeAbstraction a){ a.m(); }
          |}""")
    }

    scenario("field self use in class"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class B {
          |
          |    private int f;
          |
          |    //do we put it in the interface ?
          |    //knowledge of subclass is considered bad smell so we will not (only b heuristic)
          |    public void fieldUserThatShouldNotBeInInterface(B b){ int dummy = b.f; }
          |
          |}"""
      ) {
        val packageP = fullName2id("p")
        val classB = fullName2id("p.B")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classB),
            Interface, SupertypeAbstraction).rvalue
        val g = g0.addContains(packageP, itc)


        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("field use via parameter of self type"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class C {
          |
          |    private int f;
          |
          |    public void fieldUserThatCanBeInInterface(){ int dummy = this.f; }
          |
          |}"""
      ) {
        val packageP = fullName2id("p")
        val classC = fullName2id("p.C")


        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classC),
            Interface, SupertypeAbstraction).rvalue
        val g = g0.addContains(packageP, itc)

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("use of type member sibling by self and parameter"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |
          |    private int f;
          |
          |    public void m(int i){}
          |
          |    public void canBeInInterface(A a){ a.m(this.f); }
          |
          |    public void cannotBeInInterface(A a){ this.m(a.f); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A",
            Interface, SupertypeAbstraction).rvalue
         g0.addContains("p", itc)
        },
        """package p;
          |
          |interface A_SupertypeAbstraction {
          |    void m(int i);
          |    void canBeInInterface(A_SupertypeAbstraction a);
          |}
          |
          |class A implements A_SupertypeAbstraction{
          |
          |    private int f;
          |
          |    public void m(int i){}
          |
          |    public void canBeInInterface(A_SupertypeAbstraction a){ a.m(this.f); }
          |
          |    public void cannotBeInInterface(A a){ this.m(a.f); }
          |}"""
      )
    }

    info("super type already present")
    scenario("existing supertype - simple case"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |interface SuperA{
          |    public void mInInterface();
          |}
          |
          |class A implements SuperA {
          |    public void mInInterface(){}
          |    public void mNotInInterface(){}
          |}"""
      ) {
        val packageP = fullName2id("p")
        val classA = fullName2id("p.A")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).rvalue
        val g = g0.addContains(packageP, itc)

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        //        QuickFrame(g, "g")
        //        QuickFrame(recompiledEx.graph, "recompiled")
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }
  }


  feature("Abstract constructor") {
    scenario("static factory method") {
      compareWithExpectedAndGenerated(
        """package p;
           |class FactoryClass{}
           |class A{}
        """,
        bs => {
          import bs.{graph, idOfFullName}
          val (AccessAbstraction(factoryMethod, _), g) =
            Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A.A()",
            StaticMethod, DelegationAbstraction).rvalue
          g.addContains("p.FactoryClass", factoryMethod)
        },
        """package p;
          |class FactoryClass{ static A create(){return new A();}}
          |class A{}
        """)
    }
  }

}

