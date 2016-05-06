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

package puck.javaGraph.commutativity.redirect

import puck.TransfoRulesSpec
import puck.graph.AccessAbstraction
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations.rules.Redirection

/**
  * Created by Loïc Girault on 06/05/16.
  */
class RedirectTypeMemberUsesSpec
  extends TransfoRulesSpec {

  scenario("From method to method superType"){
    compareWithExpectedAndGenerated(
      """package p;
        |
        |interface B {
        |    void m1();
        |    void m2();
        |}
        |
        |class Bimpl implements B {
        |    public void m1(){}
        |    public void m2(){}
        |}
        |
        |class A {
        |    void m(){
        |        Bimpl b = new Bimpl();
        |        b.m1();
        |        b.m2();
        |    }
        |}""",
      bs => {
        import bs.{graph, idOfFullName}

        Redirection.redirectUsesAndPropagate(graph, ("p.A.m().Definition", "p.Bimpl.m1()"),
          AccessAbstraction("p.B.m1()", SupertypeAbstraction)).rvalue
      },
      """package p;
        |
        |interface B {
        |    void m1();
        |    void m2();
        |}
        |
        |class Bimpl implements B {
        |    public void m1(){}
        |    public void m2(){}
        |}
        |
        |class A {
        |    void m(){
        |        B b = new Bimpl();
        |        b.m1();
        |        b.m2();
        |    }
        |}""")

  }


  ignore("From method to method delegate"){

  }

  ignore("From field to ??? delegate"){
    //what should we do ?
  }

}
