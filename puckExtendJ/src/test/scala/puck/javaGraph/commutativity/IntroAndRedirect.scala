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
import puck.graph.{AccessAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.Redirection
import org.extendj.ExtendJGraphUtils.Rules.abstracter
import puck.javaGraph.nodeKind.Interface
/**
  * Created by Loïc Girault on 30/06/16.
  */
class IntroAndRedirect
  extends TransfoRulesSpec {

  scenario("From class to interface superType - return type propagation,  chained call") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class C { public void m(int i){} }
        |
        |
        |abstract class A { abstract C getC(); }
        |abstract class B { abstract A getA(); void user(){ getA().getC().m(42); } }
        |""",
      bs => {
        import bs.{graph, idOfFullName}
        (for {
          absG <- abstracter.createAbstraction(graph,
            graph getConcreteNode "p.C",
            Interface,
            SupertypeAbstraction)
          (abs, g) = absG
          AccessAbstraction(itId, _) = abs
          g2 = g.addContains("p", itId) .setName(itId, "I")
          g3 <- Redirection.redirectUsesAndPropagate(g2, ("p.B.user().Definition", "p.C.m(int)"),
            AccessAbstraction((g2, "p.I.m(int)"), SupertypeAbstraction))
        } yield g3).rvalue
      },
      """package p;
        |
        |class C implements I{ public void m(int i){} }
        |
        |interface I { void m(int i); }
        |
        |abstract class A { abstract I getC(); }
        |abstract class B { abstract A getA(); void user(){ getA().getC().m(42); } }"""
    )
  }

  scenario("From class to interface superType - bug correction") {
    compareWithExpectedAndGenerated(
      """package p;
        |class Singleton{
        | private static Singleton instance = new Singleton();
        | public static Singleton getIt(){
        |   return instance;
        | }
        | public void m(){}
        |}
        |class User {
        | private static Singleton s;
        | static {
        |   s = Singleton.getIt();
        |   s.m();
        | }
        |}
      """
      ,
      bs => {
        import bs.{graph, idOfFullName}
        (for {
          absG <- abstracter.createAbstraction(graph,
            graph getConcreteNode "p.Singleton",
            Interface,
            SupertypeAbstraction)
          (abs, g) = absG
          AccessAbstraction(itId, _) = abs
          g2 = g.addContains("p", itId) .setName(itId, "I")
          g3 <- Redirection.redirectUsesAndPropagate(g2, ("p.User.s", "p.Singleton"),
            AccessAbstraction((g2, "p.I"), SupertypeAbstraction))
        } yield g3).rvalue
      },
      """package p;
        |interface I { void m();}
        |
        |class Singleton implements I {
        | private static Singleton instance = new Singleton();
        | public static Singleton getIt(){
        |   return instance;
        | }
        | public void m(){}
        |}
        |class User {
        | private static I s;
        | static {
        |   s = Singleton.getIt();
        |   s.m();
        | }
        |}
      """
    )
  }
}
