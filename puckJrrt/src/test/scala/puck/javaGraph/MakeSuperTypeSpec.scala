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

package puck.javaGraph

import puck.graph.constraints.SupertypeAbstraction
import puck.graph.{AccessAbstraction, Type}
import puck.{Settings, AcceptanceSpec}

import puck.jastadd.ExtendJGraphUtils.{transformationRules => TR}

class MakeSuperTypeSpec extends AcceptanceSpec {
  //info("We are testing behavior of the abstraction rule, i.e. we check that the structure of the graph after rule application")

  feature("Make super type") {
    scenario("Compatible super interface") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |interface SuperA {
          |    void mInInterface();
          |}
          |
          |class A {
          |    public void mInInterface(){}
          |    public void mNotInInterface(){}
          |}""") {
        val classA = fullName2id("p.A")
        val superA = fullName2id("p.SuperA")

        val methInInterface = fullName2id("p.A.mInInterface()")
        val methNotInInterface = fullName2id("p.A.mNotInInterface()")
        val abstractMethInInterface = fullName2id("p.SuperA.mInInterface()")

        val g2 = TR.makeSuperType(graph, classA, superA)().rvalue

        assert(g2.isa(classA, superA))

        g2.abstractions(methNotInInterface) should be(empty)
        g2.abstractions(methInInterface) should contain (AccessAbstraction(abstractMethInInterface, SupertypeAbstraction))
        g2.abstractions(methInInterface).size shouldBe 1
      }
    }

    scenario("Incompatible super interface") {
      val _ = new ScenarioFactory(
        """package p;
          |interface SuperA { void m(); }
          |class A { public void notM(){} }
          |"""
      ) {
        val classA = fullName2id("p.A")
        val superA = fullName2id("p.SuperA")

        TR.makeSuperType(graph, classA, superA)(Type.errorOnImplemNotFound("A")).lvalue

      }
    }
  }

}