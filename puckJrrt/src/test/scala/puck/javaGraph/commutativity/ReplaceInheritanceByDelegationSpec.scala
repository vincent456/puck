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

import puck.TransfoRulesSpec
import puck.graph.transformations.rules.ReplaceInheritanceByDelegation
import puck.jastadd.ExtendJGraphUtils.Rules
import puck.javaGraph.nodeKind.Interface

/**
  * Created by Loïc Girault on 06/07/16.
  */
class ReplaceInheritanceByDelegationSpec
  extends TransfoRulesSpec {
  scenario("moved method not used by this") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |abstract class A { public abstract void m(); }
        |
        |class B extends A { public void m(){ System.out.println("Yeah B !"); } }
        |
        |class C extends A { public void m(){ System.out.println("Yeah C !"); } }""",
      bs => {
        import bs.{graph, idOfFullName}
        import puck.graph.ShowDG._
        (graph, graph.abstractionsMap).println

        ReplaceInheritanceByDelegation.subsToDelegate(graph, List("p.B", "p.C"),
          "p.A", "p", Interface)(Rules).rvalue
      },
      """package p;
        |
        |abstract class A { ASubDelegate aSubDelegate; public void m(){ aSubDelegate.m(); } }
        |
        |interface ASubDelegate { void m(); }
        |
        |class B implements ASubDelegate { public void m(){ System.out.println("Yeah B !"); } }
        |
        |class C implements ASubDelegate { public void m(){ System.out.println("Yeah C !"); } }"""
    )
  }
}
