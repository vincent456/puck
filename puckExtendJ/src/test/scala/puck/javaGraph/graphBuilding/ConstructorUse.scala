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

package puck.javaGraph.graphBuilding

import puck.AcceptanceSpec
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 04/04/16.
  */
class ConstructorUse extends AcceptanceSpec{

  scenario("use of constructor only") {

    //val _ = new ScenarioFactory(s"$examplesPath/A.java") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |interface I{}
        |class A implements I{}
        |
        |class B {
        |    static void m() {
        |        I i = new A();
        |    }
        |}
        |""") {

      val clazz = fullName2id("p.A")
      val ctor = fullName2id("p.A.A()")
      val user = fullName2id("p.B.m().Definition")


      assert(graph.uses(user, ctor))
      assert(graph.uses(ctor, clazz))
      assert(!graph.uses(user, clazz))
    }
  }

  scenario("use this constructor") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {
        |
        |    int x, y;
        |
        |    A(int i, int j){
        |        x = i;
        |        y = j;
        |    }
        |
        |    A(int i){ this(i, 0); }
        |}
      """) {
      val primaryCtor = fullName2id("p.A.A(int,int)")
      val secondaryCtor = fullName2id("p.A.A(int)")
      assert(graph.uses(graph.definitionOf_!(secondaryCtor), primaryCtor))
    }
  }

  scenario("use super constructor") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {
        |
        |    int x;
        |
        |    A(int i){ x = i;}
        |
        |}
        |
        |class B extends A{
        |
        |    int y;
        |
        |    B(int i, int j){
        |        super(i);
        |        y = j;
        |    }
        |
        |}
      """.stripMargin
    ) {
      val bCtor = fullName2id("p.B.B(int,int)")
      val aCtor = fullName2id("p.A.A(int)")
      assert(graph.uses(graph.definitionOf_!(bCtor), aCtor))
    }
  }
}
