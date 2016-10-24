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

package puck.javaGraph.commutativity.move

import puck.TransfoRulesSpec
import puck.Settings._
import puck.graph.{Contains, DependencyGraph, NodeId}
import puck.graph.comparison.Mapping
import puck.graph.transformations.rules.Move
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Package

/**
  * Created by Loïc Girault on 19/04/16.
  */
class MoveTypeSpec
  extends TransfoRulesSpec  {

  def createTopLevelPackage(g : DependencyGraph, name : String) : (DependencyGraph, NodeId) = {
    val (pn , g2) = g.addConcreteNode(name, Package)
    (g2.addEdge(Contains(g.root.id, pn.id)), pn.id)
  }

  scenario("Move top level class") {
    compareWithExpectedAndGenerated(
      """package p1;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |}
        |
        |class B {
        |    public void mb(){
        |        A a = new A();
        |        a.ma();
        |    }
        |}""",
      bs => {
        import bs.{graph, idOfFullName}
        val (g0, package2) = createTopLevelPackage(graph, "p2")
        Move.staticDecl(g0, "p1.A", package2).rvalue
      },
      Seq(
      """package p2;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |}""",

     """package p1;
        |
        |import p2.A;
        |
        |class B {
        |    public void mb(){
        |        A a = new A();
        |        a.ma();
        |    }
        |}"""))
  }

  scenario("Move top level class - moved type uses type of old package") {
    val _ = new ScenarioFactory(
      """package p1;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |}
        |
        |class B {
        |    public void mb(){
        |        A a = new A();
        |        a.ma();
        |    }
        |}"""){
      val classB = fullName2id("p1.B")

      val (g0, package2) = createTopLevelPackage(graph, "p2")
      val g1 = Move.staticDecl(g0, classB, package2).rvalue

      val recompiledEx = applyChangeAndMakeExample(g1, outDir)
      assert( Mapping.equals(g1, recompiledEx.graph) )

    }
  }

  scenario("Move top from different Packages - local var decl") {
    def code(aPackage : String) = Seq(
      s"""package $aPackage;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |}""",
      s"""package p2;
        |
        |import $aPackage.A;
        |
        |public class B {
        |    public void mb(){
        |        A a = new A();
        |        a.ma();
        |    }
        |}""")
    compareWithExpectedAndGenerated(
      code("p1"),
      bs => {
        import bs.{graph, idOfFullName}
        val (g0, package3) = createTopLevelPackage(graph, "p3")
        val g1 = Move.staticDecl(g0, "p1.A", package3).rvalue
        g1.removeContains(g1.rootId, "p1").removeNode("p1")._2
      },code("p3"))

  }

  scenario("Move top from different Packages - field decl") {
    val _ = new ScenarioFactory(
      """package p1;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |}""",
      """package p2;
        |
        |import p1.A;
        |
        |public class B {
        |    A a = new A();
        |    public void mb(){ a.ma(); }
        |}""") {

      val p1 = fullName2id(s"p1")
      val classA = fullName2id(s"p1.A")

      val (g0, package3) = createTopLevelPackage(graph, "p3")

      val g1 = Move.staticDecl(g0, classA, package3).rvalue

      val recompiledEx = applyChangeAndMakeExample(g1, outDir)

      val gClean = g1.removeContains(g1.rootId, p1).removeNode(p1)._2
      assert( Mapping.equals(recompiledEx.graph, gClean) )

    }

  }

  scenario("Move top between different Packages - actual parameter") {
    val _ = new ScenarioFactory(
      """package p1;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |}""",
      """package p2;
        |
        |import p1.A;
        |
        |public class B {
        |    public void mb(){ mb2(new A()); }
        |    public void mb2(A a){}
        |}""") {

      val p1 = fullName2id(s"p1")
      val classA = fullName2id(s"p1.A")

      val (g0, package3) = createTopLevelPackage(graph, "p3")

      val g1 = Move.staticDecl(g0, classA, package3).rvalue

      val recompiledEx = applyChangeAndMakeExample(g1, outDir)

      val gClean = g1.removeContains(g1.rootId, p1).removeNode(p1)._2
      assert( Mapping.equals(recompiledEx.graph, gClean) )

    }

  }



  scenario("Move top from different Packages - actual gen parameter") {

    def code(aPackage : String) : Seq[String] = Seq(
      s"""package $aPackage;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |}""",
      s"""package p2;
        |
        |import $aPackage.A;
        |
        |class B<T>{ void m(T t){} }
        |
        |class C{
        |    B<A> ba;
        |    void m(){ ba.m(new A()); }
        |}""")

    compareWithExpectedAndGenerated(code("p1"),
      bs => {
        import bs.{graph, idOfFullName}
        val (g0, package3) = createTopLevelPackage(graph, "p3")

        val g1 = Move.staticDecl(g0, "p1.A", package3).rvalue

        (g1 removeNode "p1")._2.removeEdge(Contains(graph.rootId, "p1"))

    }, code("p3"))

  }



  scenario("Move top from different Packages - actual parameter with overloading") {
    val p = "topLevelClass/classesInDifferentPackages/actualParameterWithOverloading"
    val _ = new ScenarioFactory(
      """package p1;
        |
        |import java.util.List;
        |
        |public interface TextTranslator{}
        |
        |public class ComboProperty {
        |
        |    public ComboProperty(String[] possibles, TextTranslator pTranslator) { super(); }
        |
        |    public ComboProperty(String[] possibles, List possibleTranslations) {}
        |
        |	public ComboProperty(List possibles, List possibleTranslations) {}
        |}""",
      """package p2;
        |
        |import p1.TextTranslator;
        |import p1.ComboProperty;
        |import java.util.Vector;
        |
        |public class OptionPanel implements TextTranslator {
        |
        |	private Vector getControls() {
        |
        |		Vector controls = new Vector();
        |        String[] ss = {"FreeMind.RESOURCE_LOOKANDFEEL"};
        |
        |		//controls.add(new ComboProperty(ss, this));
        |		controls.add(new ComboProperty(ss, controls));
        |
        |        return controls;
        |    }
        |}""") {

      val p1 = fullName2id("p1")
      val classA = fullName2id("p1.ComboProperty")

      val (g0, package3) = createTopLevelPackage(graph, "p3")

      val g1 = Move.staticDecl(g0, classA, package3).rvalue

      val recompiledEx = applyChangeAndMakeExample(g1, outDir)

      val gClean = g1.removeContains(g1.rootId, p1).removeNode(p1)._2
      assert( Mapping.equals(recompiledEx.graph, g1) )

      assert(true)
    }

  }

  scenario("Move top from different Packages - with static class member") {
    val _ = new ScenarioFactory(
      """package p1;
        |
        |public class A {
        |    public A(){}
        |    public void ma(){}
        |
        |    public static class InnerA {
        |        public InnerA(){}
        |        public void innerMa(){}
        |    }
        |
        |}""",
      """package p2;
        |
        |import p1.A.InnerA;
        |
        |public class B {
        |    public void mb(){
        |        InnerA ia = new InnerA();
        |        ia.innerMa();
        |    }
        |}""") {

      val p1 = fullName2id("p1")

      val classA = fullName2id("p1.A")

      val (g0, package3) = createTopLevelPackage(graph, "p3")

      val g = Move.staticDecl(g0, classA, package3).rvalue

      val recompiledEx = applyChangeAndMakeExample(g, outDir)

      val gClean = g.removeContains(g.rootId, p1).removeNode(p1)._2
      assert( Mapping.equals(recompiledEx.graph, gClean) )
    }
  }

  scenario("Move top from different Packages - static method inside") {

    def code(aPackage : String) : Seq[String] = Seq(
      s"""package $aPackage;
          |
          |public class A {
          |    static public int ma(){ return 42;}
          |}""",
      s"""package p2;
          |
          |import $aPackage.A;
          |
          |class C{
          |    void m(){ int i = A.ma(); }
          |}""")

    compareWithExpectedAndGenerated(code("p1"),
      bs => {
        import bs.{graph, idOfFullName}
        val (g0, package3) = createTopLevelPackage(graph, "p3")

        val g1 = Move.staticDecl(g0, "p1.A", package3).rvalue

        (g1 removeNode "p1")._2.removeEdge(Contains(graph.rootId, "p1"))

      }, code("p3"))

  }

  scenario("Move top from different Packages - static method inside - import static") {

    def code(aPackage : String) : Seq[String] = Seq(
      s"""package $aPackage;
          |
          |public class A {
          |    static public int ma(){ return 42;}
          |}""",
      s"""package p2;
          |
          |import static $aPackage.A.ma;
          |
          |class C{
          |    void m(){ int i = ma(); }
          |}""")

    compareWithExpectedAndGenerated(code("p1"),
      bs => {
        import bs.{graph, idOfFullName}
        val (g0, package3) = createTopLevelPackage(graph, "p3")

        val g1 = Move.staticDecl(g0, "p1.A", package3).rvalue

        (g1 removeNode "p1")._2.removeEdge(Contains(graph.rootId, "p1"))

      }, code("p3"))

  }
}
