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

import puck.graph.transformations.rules.{Move, Redirection}
import puck.graph.{AccessAbstraction, DelegationAbstraction, DependencyGraph, Factory, LoggedTG}
import puck.TransfoRulesSpec

/**
  * Created by Loïc Girault on 04/12/15.
  */
class MoveAndRedirect
  extends TransfoRulesSpec {

  feature("confluence of move and redirect"){

    val initialCode =
      """package p;
        |
        |class Factory{ void m(){} }
        |
        |class B { static B createB(){ return new B(); } }
        |
        |class A { void m() { B b = new B(); } }"""

    val expectedCode =
      """package p;
        |
        |class Factory{ static B createB(){ return new B(); } }
        |
        |class B { }
        |
        |class A { void m() { B b = Factory.createB(); } }"""


    scenario("move factory then redirect") {
      compareWithExpectedAndGenerated(initialCode,
        s => {
          import s.{graph, idOfFullName}

          val g : DependencyGraph =
            graph.addAbstraction("p.B.B()", AccessAbstraction("p.B.createB()", DelegationAbstraction))
              .setRole("p.B.createB()", Some(Factory("p.B.B()")))

          // 1ère manière
          val g0 = Move.staticDecl(g, "p.B.createB()", "p.Factory").rvalue

          Redirection.redirectUsesAndPropagate(g0, ("p.A.m().Definition", "p.B.B()"),
            AccessAbstraction("p.B.createB()", DelegationAbstraction)).rvalue

          // 2ème manière
//          val ltg : LoggedTG =
//            Move.staticDecl(g, "p.B.createB()", "p.Factory").flatMap{
//              g0 =>
//                Redirection.redirectUsesAndPropagate(g0, ("p.A.m().Definition", "p.B.B()"),
//                  AccessAbstraction("p.B.createB()", DelegationAbstraction)) map {
//                  g1 => g1
//                }
//            }
//          ltg.rvalue

          // 3ème manière
//          val ltg : LoggedTG =
//            for {
//              g0 <- Move.staticDecl(g, "p.B.createB()", "p.Factory")
//
//              g1 <- Redirection.redirectUsesAndPropagate(g0, ("p.A.m().Definition", "p.B.B()"),
//                AccessAbstraction("p.B.createB()", DelegationAbstraction))
//            } yield g1
//          ltg.rvalue




        }, expectedCode)
    }

    scenario("redirect then move factory ") {
      compareWithExpectedAndGenerated(initialCode,
        bs => {
          import bs.{graph, idOfFullName}

          val g =
          graph.addAbstraction("p.B.B()", AccessAbstraction("p.B.createB()", DelegationAbstraction))
            .setRole("p.B.createB()", Some(Factory("p.B.B()")))

        val ltg : LoggedTG =
          for {
            g0 <- Redirection.redirectUsesAndPropagate(g, ("p.A.m().Definition", "p.B.B()"),
              AccessAbstraction("p.B.createB()", DelegationAbstraction))

            g1 <- Move.staticDecl(g0, "p.B.createB()", "p.Factory")

          } yield g1

          ltg.rvalue
        }, expectedCode)
    }

  }
}
