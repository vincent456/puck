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

package puck.javaGraph.transfoRules.redirect

import puck.{Settings, AcceptanceSpec}
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.{AccessAbstraction, Uses}
import puck.graph.transformations.rules.Redirection
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 24/03/16.
  */
class RedirectTypeMemberSpec
  extends AcceptanceSpec {

    scenario("From method to method superType"){
      val _ = new ScenarioFactory(
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
          |
          |    void m(){
          |        Bimpl b = new Bimpl();
          |        b.m1();
          |        b.m2();
          |    }
          |}"""
      ) {
        val mUsed = fullName2id("p.Bimpl.m1()")
        val mAbs = fullName2id("p.B.m1()")
        val cUsed = fullName2id("p.Bimpl")
        val cUsedCtor = fullName2id("p.Bimpl.Bimpl()")

        val otherMused = fullName2id("p.Bimpl.m2()")
        val otherMabs = fullName2id("p.B.m2()")
        val cAbs = fullName2id("p.B")

        val userDecl = fullName2id("p.A.m()")
        val userDef = fullName2id("p.A.m().Definition")

        val useOfImplClass = Uses(userDef, cUsed)
        val useOfctor = Uses(userDef, cUsedCtor)
        val useOfmeth = Uses(userDef, mUsed)
        val useOfOtherMeth = Uses(userDef, otherMused)

        val useOfAbsClass = Uses(userDef, cAbs)
        val useOfmethAbs = Uses(userDef, mAbs)
        val useOfOtherMethAbs = Uses(userDef, otherMabs)

        assert(useOfImplClass existsIn graph)
        assert(useOfctor existsIn graph)
        assert(useOfmeth existsIn graph)
        assert(useOfOtherMeth existsIn graph)

        assert(! (useOfAbsClass existsIn graph))
        assert(! (useOfmethAbs existsIn graph))
        assert(! (useOfOtherMethAbs existsIn graph))

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            useOfmeth, AccessAbstraction(mAbs, SupertypeAbstraction)).rvalue


        assert(! (useOfImplClass existsIn g))
        assert(useOfctor existsIn g)

        assert(! (useOfmeth existsIn g))
        assert(! (useOfOtherMeth existsIn g))

        assert(useOfAbsClass existsIn g)
        assert(useOfmethAbs existsIn g)
        assert(useOfOtherMethAbs existsIn g)

      }
    }

    ignore("From method to method delegate"){

    }

    ignore("From field to ??? delegate"){
      //what should we do ?
    }



}
