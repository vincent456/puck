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

import puck.graph._
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Move}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Field
import puck.AcceptanceSpec
import puck.javaGraph.nodeKind.Package

object MoveSpec {
  def createTopLevelPackage(g : DependencyGraph, name : String) : (DependencyGraph, NodeId) = {
    val (pn , g2) = g.addConcreteNode(name, Package)
    (g2.addEdge(Contains(g.root.id, pn.id)), pn.id)
  }
}
import MoveSpec.createTopLevelPackage
class MoveSpec
  extends AcceptanceSpec {

  feature("Move class") {

    scenario("Move top level class") {
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
          |}"""
      ) {




        assert(graph.container("p1.A").value == fullName2id("p1"))
        assert(graph.uses("p1.B.mb().Definition", "p1.A"))
        assert(graph.uses("p1.B.mb().Definition", "p1.A.ma()"))


        val (g1, package2) = createTopLevelPackage(graph, "p2")

        val g2 = Move.staticDecl(g1, "p1.A", package2).rvalue
        assert(g2.container("p1.A").value == package2)
        assert(g2.uses("p1.B.mb().Definition", "p1.A"))
        assert(g2.uses("p1.B.mb().Definition", "p1.A.ma()"))

      }
    }

    scenario("Move top from different packages ") {
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
          |    public void mb(){
          |        A a = new A();
          |        a.ma();
          |    }
          |}"""
      ) {
        val package1 = fullName2id("p1")

        val classA = fullName2id("p1.A")
        val methADecl = fullName2id("p1.A.ma()")

        val classB = fullName2id("p2.B")
        val methBDecl = fullName2id("p2.B.mb()")
        val methBDef =  graph.definitionOf_!(methBDecl)


        assert(graph.container(classA).value == package1)
        assert(graph.uses(methBDef, classA))
        assert(graph.uses(methBDef, methADecl))

        val (g1, package3) = createTopLevelPackage(graph, "p3")
        val g2 = Move.staticDecl(g1, classA, package3).rvalue

        assert(g2.container(classA).value == package3)
        assert(g2.uses(methBDef, classA))
        assert(g2.uses(methBDef, methADecl))

      }

    }

  }


  def assertIsArrowAndUncurry(t : Type) : Arrow = {
    t match {
      case a : Arrow => a.uncurry
      case _ => assert(false)
        Arrow(NamedType(0), NamedType(0))
    }
  }
  def assertIsTupleAndGetSize : Type => Int = {
    case Tuple(ts) => ts.size
    case _ => assert(false)
      0
  }

  feature("Move method"){

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

        val classA = fullName2id("p.A")
        val methToMove = fullName2id("p.A.methodToMove()")
        val methUserDecl = fullName2id("p.C.user()")
        val methUserDef = fullName2id("p.C.user().Definition")

        val classB = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)

        assert(graph.uses(methUserDef, classA))
        assert(graph.uses(methUserDef, methToMove))

        val g2 = Move.typeMember(graph, List(methToMove), classB, Some(CreateParameter)).rvalue
        assert(g2.container(methToMove).value == classB)
        assert(g2.uses(methUserDef, methToMove))
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
          |class B{ }"""){

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser()")
        val methUser = fullName2id("p.A.mUser().Definition")
        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))
        graph.parametersOf(methUserDecl) shouldBe empty

        val g2 = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter)).rvalue
        g2.content(classA).size shouldBe (graph.content(classA).size - 1)
        assert(g2.container(methToMove).value == newHostClass)
        assert(g2.uses(methUser, methToMove))

        val paramList = g2.parametersOf(methUserDecl)
        paramList.size shouldBe 1
        assert(g2.uses(paramList.head, newHostClass))
      }
    }

    scenario("move method used by this - keep reference with Field"){
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

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser()")
        val methUser = fullName2id("p.A.mUser().Definition")
        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))
        assert(! graph.uses(methUser, newHostClass))

        val g2 = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field))).rvalue
        //quickFrame(g2)
        val ma2Delegate =
          g2.content(classA).find{
            id =>
              g2.getConcreteNode(id).name == "b_delegate"
          }.value

        assert(g2.container(methToMove).value == newHostClass)

        assert(g2.uses(methUser, methToMove))
        assert(g2.uses(ma2Delegate, newHostClass))
        assert(g2.uses(methUser, ma2Delegate))
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


        val selfUse : NodeIdP = ("p.A","p.A")
        assert(graph.uses("p.A.mUser().Definition", "p.A.mUsedToMove()"))
        assert(graph.uses("p.A.mUser().Definition", "p.A.mUsedOther()"))
        graph.typeUsesOf(Uses("p.A.mUser().Definition", "p.A.mUsedToMove()")) should contain (selfUse)
        graph.typeUsesOf(Uses("p.A.mUser().Definition", "p.A.mUsedOther()")) should contain (selfUse)

        val g2 = Move.typeMember(graph, List("p.A.mUsedToMove()"), "p.B", Some(CreateTypeMember(Field))).rvalue



        assert(g2.uses("p.A.mUser().Definition", "p.A.mUsedToMove()"))
        assert(g2.uses("p.A.mUser().Definition", "p.A.mUsedOther()"))

        assert(g2.uses((g2, "p.A.b_delegate"), "p.B"))

        val newTypeUse : NodeIdP = ((g2, "p.A.b_delegate"), "p.B")

        g2.typeUsesOf(Uses("p.A.mUser().Definition", "p.A.mUsedToMove()")) should contain (newTypeUse)
        g2.typeUsesOf(Uses("p.A.mUser().Definition", "p.A.mUsedOther()")) should contain (selfUse)

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



        assert(graph.container("p.A.methodToMove()").value == fullName2id("p.A"))

        assert(graph.uses("p.A.mUser1().Definition", "p.A.methodToMove()"))
        graph.parametersOf("p.A.mUser1()").size shouldBe 0

        assert(graph.uses("p.A.mUser2().Definition", "p.A.methodToMove()"))
        graph.parametersOf("p.A.mUser2()").size shouldBe 0

        val g2 = Move.typeMember(graph, List("p.A.methodToMove()"), "p.B", Some(CreateParameter)).rvalue

        val params1 = g2.parametersOf("p.A.mUser1()")
        params1.size shouldBe 1
        val params2 = g2.parametersOf("p.A.mUser2()")
        params2.size shouldBe 1

        assert(g2.container("p.A.methodToMove()").value == fullName2id("p.B"))

        assert(g2.uses("p.A.mUser1().Definition", "p.A.methodToMove()"))
        assert(g2.uses(params1.head, "p.B"))

        assert(g2.uses("p.A.mUser2().Definition", "p.A.methodToMove()"))
        assert(g2.uses(params2.head, "p.B"))

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

        val classA = fullName2id("p.A")
        val methUser1Decl = fullName2id("p.A.mUser1()")
        val methUser2Decl = fullName2id("p.A.mUser2()")

        val methUser1 = fullName2id("p.A.mUser1().Definition")
        val methUser2 = fullName2id("p.A.mUser2().Definition")

        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser1, methToMove))
        assert(graph.uses(methUser2, methToMove))

        val g2 =
          Move.typeMember(graph, List(methToMove), newHostClass,
            Some(CreateTypeMember(Field))).rvalue

        val methToMoveDelegateList =
          g2.content(classA).filter {
            id =>
              g2.getConcreteNode(id).name startsWith "b_delegate"
          }

        assert(methToMoveDelegateList.size == 1)
        val methToMoveDelegate = methToMoveDelegateList.head

        assert(g2.container(methToMove).value == newHostClass)

        assert(g2.uses(methUser2, methToMove))
        assert(g2.uses(methToMoveDelegate, newHostClass))
        assert(g2.uses(methUser2, methToMoveDelegate))

        assert(g2.uses(methUser1, methToMove))
        assert(g2.uses(methToMoveDelegate, newHostClass))
        assert(g2.uses(methUser1, methToMoveDelegate))

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

        val rootPackage = fullName2id("p")

        val classA = fullName2id("p.A")
        val methMa = fullName2id("p.A.ma(B)")
        val methUser = fullName2id("p.C.mc()")

        val classB = fullName2id("p.B")

        val methMaNode = graph.getConcreteNode(methMa)

        val Arrow(in @ Tuple(_), _) = graph.styp(methMa).value

        assert(1 == in.length && in.ids.contains(classB))

        assert(graph.container(methMa).value == classA)
        assert(graph.uses(methUser, methMa))

        val g2 = Move.typeMember(graph, List(methMa), classB).rvalue

        assert(g2.container(methMa).value == classB)
        assert(g2.uses(methUser, methMa))

        val Arrow(in2 @ Tuple(_), _) = g2.styp(methMa).value
        assert(0 == in2.length)
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

        val currentHost = fullName2id("p.A")
        val methToMoveDecl = fullName2id("p.A.methodToMove()")
        val methToMoveDef = fullName2id("p.A.methodToMove().Definition")
        val methUsed = fullName2id("p.A.mUsed()")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMoveDecl).value == currentHost)
        assert(graph.uses(methToMoveDef, methUsed))
        graph.parametersOf(methToMoveDecl).size shouldBe 0

        val g2 = Move.typeMember(graph, List(methToMoveDecl), newHostClass, Some(CreateParameter)).rvalue
        g2.content(currentHost).size shouldBe (graph.content(currentHost).size - 1)

        assert(g2.container(methToMoveDecl).value == newHostClass)
        assert(g2.uses(methToMoveDef, methUsed))
        val params = g2.parametersOf(methToMoveDecl)
        params.size shouldBe 1
        assert(g2.uses(params.head, currentHost))
      }

    }
  }

  feature("Move methods"){

    scenario("one of the moved is used by another") {
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

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser()")
        val methUser = fullName2id("p.A.mUser().Definition")

        val methToMoveDecl = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")



        val g2 = Move.typeMember(graph, List(methToMoveDecl, methUserDecl), newHostClass, Some(CreateParameter)).rvalue

        g2.content(classA).size shouldBe (graph.content(classA).size - 2)
        assert(g2.container(methToMoveDecl).value == newHostClass)
        assert(g2.container(methUserDecl).value == newHostClass)
        assert(g2.uses(methUser, methToMoveDecl))
        assert(!g2.uses(methUserDecl, newHostClass))
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

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser()")
        val methUser = fullName2id("p.A.mUser().Definition")
        val methToMove1 = fullName2id("p.A.methodToMove1()")
        val methToMove2 = fullName2id("p.A.methodToMove2()")

        val newHostClass = fullName2id("p.B")

        val numArgs = graph.parametersOf(methUserDecl).size

        val g2 = Move.typeMember(graph, List(methToMove1, methToMove2), newHostClass, Some(CreateParameter)).rvalue

        g2.content(classA).size shouldBe (graph.content(classA).size - 2)
        g2.parametersOf(methUserDecl).size  shouldBe (numArgs + 1)
        assert(g2.container(methToMove1).value == newHostClass)
        assert(g2.container(methToMove2).value == newHostClass)

        assert(g2.uses(methUser, methToMove1))
        assert(g2.uses(methUser, methToMove2))
      }

    }

  }
}
