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

import puck.Settings._
import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Move}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Field
import puck.AcceptanceSpec

class MoveMethodSpec extends AcceptanceSpec {


  feature("Move one method"){

    scenario("moved method not used by this"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A { public void methodToMove(){} }
          |
          |class B { }
          |
          |class C {
          |    public void user(){
          |        A a = new A();
          |        a.methodToMove();
          |    }
          |}"""
      ){

        val g = Move.typeMember(graph, List("p.A.methodToMove()"), "p.B",
          Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }

    }


    scenario("move method used by this - keep reference with parameter"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser(){ methodToMove(); }
          |
          |    public void methodToMove(){}
          |}
          |
          |class B{ }"""
      ){


        val g = Move.typeMember(graph, List("p.A.methodToMove()"), "p.B",
          Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this - keep reference with Field"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser(){ m(); }
          |
          |    public void m(){}
          |}
          |
          |class B{ }"""
      ){

        val g = Move.typeMember(graph, List[NodeId]("p.A.m()"), "p.B",
                  Some(CreateTypeMember(Field))).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this - user also via self another method that will not be moved "){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |    void mUser(){
          |        mUsedToMove();
          |        mUsedOther();
          |    }
          |
          |    void mUsedToMove(){}
          |    void mUsedOther(){}
          |}
          |
          |class B{ }"""
      ) {

        val g = Move.typeMember(graph, List("p.A.mUsedToMove()"), "p.B",
          Some(CreateTypeMember(Field))).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("move method used by this several times - keep reference with Parameter"){


      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser1(){ methodToMove(); }
          |
          |    public void mUser2(){ methodToMove(); }
          |
          |    public void methodToMove(){}
          |}
          |
          |class B{ }"""
      ){

        val g = Move.typeMember(graph, List("p.A.methodToMove()"), "p.B", Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this several times - keep reference with Field"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser1(){ methodToMove(); }
          |
          |    public void mUser2(){ methodToMove(); }
          |
          |    public void methodToMove(){}
          |}
          |
          |class B{ }"""
      ){

        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g =
          Move.typeMember(graph, List(methToMove), newHostClass,
            Some(CreateTypeMember(Field))).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    ignore("Move method not used by this to class of a parameter"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A { public void methodToMove(){} }
          |
          |class B { }
          |
          |class C {
          |    public void user(){
          |        A a = new A();
          |        a.methodToMove();
          |    }
          |}"""
      ){

        val methMa = fullName2id("p.A.ma__B")
        val classB = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methMa), classB).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }


    scenario("moved method uses this - keep reference with parameter "){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public A(){}
          |
          |    public void mUsed(){}
          |
          |    public void methodToMove(){ mUsed();}
          |}
          |
          |class B{ }"""
      ){

        val methToMoveDecl = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMoveDecl), newHostClass, Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }

    }
  }

  feature("Move several methods"){

    scenario("one of the moved method is used by another") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser(){ methodToMove(); }
          |
          |    public void methodToMove(){}
          |}
          |
          |class B{ }"""
      ) {

        val methUserDecl = fullName2id("p.A.mUser()")
        val methToMoveDecl = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMoveDecl, methUserDecl), newHostClass, Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }

    }

    scenario("two moved method both used by an unmoved one") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser(){
          |        methodToMove1();
          |        methodToMove2();
          |    }
          |
          |    public void methodToMove1(){}
          |    public void methodToMove2(){}
          |}
          |
          |class B{ }"""
      ) {

        val methToMove1 = fullName2id("p.A.methodToMove1()")
        val methToMove2 = fullName2id("p.A.methodToMove2()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove1, methToMove2), newHostClass, Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }
  }

  feature("Move static method"){
    scenario("unused factory"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |    A(){}
          |
          |    static A createA(){return new A();}
          |
          |}
          |
          |class Client {
          |    void m(){ A a = new A(); }
          |}"""
      ) {

        val ctor = fullName2id("p.A.A()")
        val factory = fullName2id("p.A.createA()")

        val client = fullName2id("p.Client")

        val g = graph.setRole(factory, Some(Factory(ctor)))

        val g1 = Move.typeMember(g, List(factory), client, None).rvalue

        val recompiledEx = applyChangeAndMakeExample(g1, outDir)
        assert( Mapping.equals(g1, recompiledEx.graph) )

      }
    }

    scenario("used factory moved in client"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |    A(){}
          |
          |    static A createA(){return new A();}
          |}
          |
          |class Factory{ }
          |
          |class Client { void m(){ A a = A.createA(); } }"""
      ){



        val g = graph.setRole("p.A.createA()", Some(Factory("p.A.A()")))

        val g1 = Move.staticDecl(g, "p.A.createA()", "p.Client").rvalue

        val recompiledEx = applyChangeAndMakeExample(g1, outDir)
        assert( Mapping.equals(g1, recompiledEx.graph) )

      }
    }
    scenario("used factory moved in outsider host"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |    A(){}
          |
          |    static A createA(){return new A();}
          |}
          |
          |class Factory{ }
          |
          |class Client { void m(){ A a = A.createA(); } }"""
      ) {

        val g = graph.setRole("p.A.createA()", Some(Factory("p.A.A()")))

        val g1 = Move.staticDecl(g, "p.A.createA()", "p.Factory").rvalue

        val recompiledEx = applyChangeAndMakeExample(g1, outDir)
        assert( Mapping.equals(g1, recompiledEx.graph) )

      }
    }
  }
}
