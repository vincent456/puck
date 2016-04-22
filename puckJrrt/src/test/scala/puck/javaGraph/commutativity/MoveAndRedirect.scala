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

import puck.Settings._
import puck.graph.comparison.Mapping
import puck.graph.transformations.rules.{Redirection, Move}
import puck.graph.{LoggedTG, Uses, Factory, AccessAbstraction}
import puck.graph.constraints.DelegationAbstraction
import puck.javaGraph.ScenarioFactory
import puck.AcceptanceSpec

/**
  * Created by Loïc Girault on 04/12/15.
  */
class MoveAndRedirect
  extends AcceptanceSpec {

  feature("confluence of move and redirect"){

    scenario("move factory then redirect") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class Factory{ }
          |
          |class B {
          |    B(){}
          |    static B createB(){ return new B(); }
          |}
          |
          |class A { void m() { B b = new B(); } }"""
      ) {
        val ctor = fullName2id("p.B.B()")
        val factoryMethod = fullName2id("p.B.createB()")
        val factoryClass = fullName2id("p.Factory")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g =
          graph.addAbstraction(ctor, AccessAbstraction(factoryMethod, DelegationAbstraction))
              .setRole(factoryMethod, Some(Factory(ctor)))

        val ltg : LoggedTG =
        for{
          g0 <- Move.typeMember(g, List(factoryMethod), factoryClass, None)

          g1 <- Redirection.redirectUsesAndPropagate(g0, Uses(callerDef, ctor),
            AccessAbstraction(factoryMethod, DelegationAbstraction))
        } yield g1

        val g2 = ltg.rvalue

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    scenario("redirect then move factory ") {
      val _ = new ScenarioFactory(
        """package p;
          |
          |class Factory{ }
          |
          |class B {
          |    B(){}
          |    static B createB(){ return new B(); }
          |}
          |
          |class A { void m() { B b = new B(); } }"""
      ) {
        val ctor = fullName2id("p.B.B()")
        val factoryMethod = fullName2id("p.B.createB()")
        val factoryClass = fullName2id("p.Factory")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g =
          graph.addAbstraction(ctor, AccessAbstraction(factoryMethod, DelegationAbstraction))
            .setRole(factoryMethod, Some(Factory(ctor)))

        val ltg : LoggedTG =
          for{
            g0 <- Redirection.redirectUsesAndPropagate(g, Uses(callerDef, ctor),
              AccessAbstraction(factoryMethod, DelegationAbstraction))

            g1 <- Move.typeMember(g0, List(factoryMethod), factoryClass, None)
          } yield g1

        val g2 = ltg.rvalue

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

  }
}
