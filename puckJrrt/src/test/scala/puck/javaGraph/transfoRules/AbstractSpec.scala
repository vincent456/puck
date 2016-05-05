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

 package puck.javaGraph.transfoRules

import puck.graph.{AccessAbstraction,  DependencyGraph, Uses, Write}
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.ScenarioFactory
import puck.jastadd.ExtendJGraphUtils.{Rules => Rules}
import puck.javaGraph.nodeKind.Interface
import puck.AcceptanceSpec

class AbstractSpec extends AcceptanceSpec {

  feature("Abstract class into interface"){

    info("no pre-existing super type")
    scenario("simple case - method without args"){
      val _ = new ScenarioFactory(
        """package p;
          |class A {
          |    private int f;
          |    public void m(){}
          |}"""
      ) {
        assert( graph.directSuperTypes("p.A").isEmpty )

        assert( graph.abstractions("p.A").isEmpty )
        assert( graph.abstractions("p.A.m()").isEmpty )
        assert( graph.abstractions("p.A.f").isEmpty )


        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
            Interface, SupertypeAbstraction).rvalue
        val g = g0.addContains("p", itc)
        assert( g.isa("p.A", itc) )

        g.abstractions("p.A").size shouldBe 1
        g.abstractions("p.A.m()").size shouldBe 1
        assert( g.abstractions("p.A.f").isEmpty ,
          "Field cannot be exposed in an interface")
      }

    }

    scenario("simple case - method with one arg"){
      val _ = new ScenarioFactory(
        """package p;
          |class B {}
          |class A {
          |    public void m(B b){}
          |}"""
      ) {

        assert( graph.directSuperTypes("p.A").isEmpty )

        assert( graph.abstractions("p.A").isEmpty )
        assert( graph.abstractions("p.A.m(B)").isEmpty )

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
            Interface, SupertypeAbstraction).rvalue
        val g = g0.addContains("p", itc)
        assert( g.isa("p.A", itc) )

        assert( DependencyGraph.findElementByName(g, "p.A.m(B)").nonEmpty )

        g.abstractions("p.A").size shouldBe 1
        g.abstractions("p.A.m(B)").size shouldBe 1
      }

    }

    scenario("method self use in class"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |    public void m(){}
          |    public void methodUser(A a){ a.m(); }
          |}"""
      ) {
        val classA = fullName2id("p.A")
        val methM = fullName2id("p.A.m()")
        val methMUserDecl = fullName2id("p.A.methodUser(A)")
        val theParam = fullName2id("p.A.methodUser(A).a")
        val methMUserDef = fullName2id("p.A.methodUser(A).Definition")

        assert( graph.directSuperTypes(classA).isEmpty )

        assert( graph.uses(theParam, classA) )
        assert( graph.uses(methMUserDef, methM) )

        assert( graph.abstractions(classA).isEmpty )
        assert( graph.abstractions(methM).isEmpty )
        assert( graph.abstractions(methMUserDecl).isEmpty )
        assert( graph.abstractions(methMUserDef).isEmpty )


        val (AccessAbstraction(itc, _), g) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).rvalue

        assert( g.isa(classA, itc) )

        g.abstractions(classA).size shouldBe 1
        g.abstractions(methM).size shouldBe 1
        g.abstractions(methMUserDecl).size shouldBe 1
        assert( g.abstractions(methMUserDef).isEmpty )


        val AccessAbstraction(methMAbs, _) = g.abstractions(methM).head

        assert( g.uses(theParam, itc) )
        assert( g.uses(methMUserDef, methMAbs) )
      }


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
        val classB = fullName2id("p.B")
        val field = fullName2id("p.B.f")

        val fieldUserThatShouldNotBeInInterfaceDecl =
          fullName2id(s"p.B.fieldUserThatShouldNotBeInInterface(B)")
        val theParam =  fullName2id(s"p.B.fieldUserThatShouldNotBeInInterface(B).b")
        val fieldUserThatShouldNotBeInInterfaceDef =
          fullName2id(s"p.B.fieldUserThatShouldNotBeInInterface(B).Definition")

        assert( graph.directSuperTypes(classB).isEmpty )

        assert( graph.uses(theParam, classB) )
        assert( graph.uses(fieldUserThatShouldNotBeInInterfaceDef, field) )

        assert( graph.abstractions(classB).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatShouldNotBeInInterfaceDecl).isEmpty )


        assert( !Rules.abstracter.canBeAbstracted(graph,
          graph.getConcreteNode(fieldUserThatShouldNotBeInInterfaceDecl),
          graph.getConcreteNode(classB),
          SupertypeAbstraction))

        val (AccessAbstraction(itc, _), g) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classB),
            Interface, SupertypeAbstraction).rvalue
        assert( g.isa(classB, itc) )

        g.abstractions(classB).size shouldBe 1
        assert( g.abstractions(field).isEmpty , "Field cannot be exposed in an interface")
        assert( g.abstractions(fieldUserThatShouldNotBeInInterfaceDecl).isEmpty,
          "Method use concrete class field, should not be abstracted")

        assert( g.uses(theParam, classB) )
        assert( g.uses(fieldUserThatShouldNotBeInInterfaceDef, field) )


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
        val classC = fullName2id("p.C")
        val field = fullName2id("p.C.f")

        val fieldUserThatCanBeInInterfaceDecl =
          fullName2id("p.C.fieldUserThatCanBeInInterface()")

        val fieldUserThatCanBeInInterfaceDef =
          fullName2id(s"p.C.fieldUserThatCanBeInInterface().Definition")

        assert( graph.directSuperTypes(classC).isEmpty )

        //assert( !graph.uses(fieldUserThatCanBeInInterface, classC) )
        assert( graph.uses(fieldUserThatCanBeInInterfaceDef, field) )

        assert( graph.abstractions(classC).isEmpty )
        assert( graph.abstractions(field).isEmpty )
        assert( graph.abstractions(fieldUserThatCanBeInInterfaceDecl).isEmpty )

        val (AccessAbstraction(itc, _), g) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classC),
            Interface, SupertypeAbstraction).rvalue

        assert( g.isa(classC, itc) )

        g.abstractions(classC).size shouldBe 1
        assert( g.abstractions(field).isEmpty ,
          "Field cannot be exposed in an interface")
        g.abstractions(fieldUserThatCanBeInInterfaceDecl).size shouldBe 1

        //assert( graph.uses(fieldUserThatCanBeInInterface, classC) )
        assert( graph.uses(fieldUserThatCanBeInInterfaceDef, field) )

      }
    }

    scenario("use of type member sibling by self and parameter"){
      val _ = new ScenarioFactory(
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
          |}"""
      ){
        val classA = fullName2id("p.A")
        val field = fullName2id("p.A.f")
        val usedMeth = fullName2id("p.A.m(int)")

        val methCanBeInInterface = fullName2id("p.A.canBeInInterface(A)")
        val methCannotBeInInterface = fullName2id("p.A.cannotBeInInterface(A)")

        val (AccessAbstraction(itc, _), g) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).rvalue

        assert( g.isa(classA, itc))

        assert( g.abstractions(methCannotBeInInterface).isEmpty)
        g.abstractions(methCanBeInInterface).size shouldBe 1


      }
    }

    ignore("use of type member sibling by local variable and parameter"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    private int f;
          |
          |    public void m(int i){}
          |
          |    public void canBeInInterface(A a1){
          |        A a2 = new A();
          |        a1.m(a2.f);
          |    }
          |
          |    public void cannotBeInInterface(A a1){
          |        A a2 = new A();
          |        a2.m(a1.f);
          |    }
          |}"""
      ){
        val classA = fullName2id("p.A")
        val field = fullName2id("p.A.f")
        val usedMeth = fullName2id("p.A.m(int)")

        val methCanBeInInterface = fullName2id("p.A.canBeInInterface(A)")
        val methCannotBeInInterface = fullName2id("p.A.cannotBeInInterface(A)")

        val (AccessAbstraction(itc, _), g) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).rvalue

        assert( g.isa(classA, itc))

        assert( g.abstractions(methCannotBeInInterface).isEmpty)
        g.abstractions(methCanBeInInterface).size shouldBe 1

      }
    }

    ignore("abstract class into interface - cyclic uses in class (recursion)"){}
    ignore("abstract class into interface - super interface existing"){}
    ignore("abstract class into interface - super class existing"){}


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
        val classA = fullName2id("p.A")
        val methInInterface = fullName2id("p.A.mInInterface()")
        val methNotInInterface = fullName2id("p.A.mNotInInterface()")

        val superA = fullName2id("p.SuperA")
        val absMethInInterface = fullName2id("p.SuperA.mInInterface()")


        assert( graph.isa(classA, superA) )


        val (AccessAbstraction(itc, _), g) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classA),
            Interface, SupertypeAbstraction).rvalue

        assert( g.isa(classA, itc) )
        assert( g.isa(itc, superA) )

        val absMeths = g.content(itc)
        val mInBothInterface  =
          absMeths.find(g.getConcreteNode(_).name == "mInInterface").value
        val mInNewInterface =
          absMeths.find(g.getConcreteNode(_).name == "mNotInInterface").value

        g.abstractions(methInInterface)  should contain (AccessAbstraction(mInBothInterface, SupertypeAbstraction))
        g.abstractions(mInBothInterface) should contain (AccessAbstraction(absMethInInterface, SupertypeAbstraction))
        g.abstractions(methNotInInterface)  should contain (AccessAbstraction(mInNewInterface, SupertypeAbstraction))

      }
    }
  }

  feature("Intro initializer"){
    scenario("one constructor one initialized field"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class F{}
          |
          |public class A {
          |    private F f = new F();
          |    public A(){}
          |}"""
      ) {
        val classA = fullName2id("p.A")
        val classActor = fullName2id("p.A.A()")
        val initializedField = fullName2id("p.A.f")

        val fieldDef = fullName2id(s"p.A.f.Definition")
        val fieldType = fullName2id("p.F")
        val fieldTypeCtor = fullName2id("p.F.F()")

        assert(graph.uses(initializedField, fieldType))
        assert(graph.uses(fieldDef, fieldTypeCtor))

        val (initializer, g) = Rules.intro.initializer(graph, classA)
        val initializerDef = g.definitionOf_!(initializer)

        assert(g.uses(initializedField, fieldType))
        assert(g definitionOf initializedField isEmpty)

        assert( Uses(initializerDef, initializedField, Some(Write)) existsIn g)
        assert( Uses(initializerDef, fieldTypeCtor) existsIn g)
        assert( Uses(g definitionOf_! classActor, initializer) existsIn g)

      }
    }
  }

  feature("Abstract constructor into factory method"){
    ignore("one constructor no initialized field"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class F{}
          |
          |public class A {
          |
          |    private F f; //not initialized means not initialized outside of the constructor
          |    public A(){ f = new F(); }
          |
          |}"""
      ) {

      }
    }

    ignore("one constructor one initialized field"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class F{}
          |
          |public class A {
          |
          |    private F f = new F();
          |    public A(){}
          |}"""
      ) {

      }
    }

    ignore("two constructors one initialized field"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class F{}
          |class G{}
          |public class A {
          |
          |    private F f = new F();
          |    private G g;
          |    public A() { g = new G(); }
          |    public A(G g){ this.g = g; }
          |}"""
      ) {

      }
    }

  }
}
