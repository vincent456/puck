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
import puck.graph.{NamedType, NodeIdP}
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 06/04/16.
  */
class UsesRegistration extends AcceptanceSpec{

  //3 cas de typeUse vers thisClass
  // sig uses : param type
  // body uses : local var or static access
  // thisClass uses : call a sibling method or field
  scenario("this use explicit") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {
        |
        |    void m(){}
        |
        |    void mUserViaThis(){ this.m(); }
        |
        |    void mUserViaParameter(A a){ a.m(); }
        |
        |}
      """
    ) {

      //methodUse
      assert( graph.uses("p.A.mUserViaThis().Definition", "p.A.m()") )
      assert( ! graph.uses("p.A.mUserViaThis()", "p.A.m()") )

      //typeUse
      assert( graph.uses("p.A", "p.A") )

      assert( graph.uses("p.A.mUserViaParameter(A).Definition", "p.A.m()") )
      assert( !graph.uses("p.A.mUserViaParameter(A)", "p.A.m()") )

      assert( graph.uses("p.A.mUserViaParameter(A).a", "p.A") )
      assert( !graph.uses("p.A.mUserViaParameter(A)", "p.A") )

    }
  }


  scenario("this use implicit") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A {
        |
        |    void m(){}
        |
        |    void mUserViaThis(){ m(); }
        |
        |    void mUserViaParameter(A a){ a.m(); }
        |}
      """) {
      val clazz = fullName2id("p.A")
      val methM = fullName2id("p.A.m()")
      val mUserViaThis = fullName2id("p.A.mUserViaThis()")
      val mUserViaParameter = fullName2id("p.A.mUserViaParameter(A)")
      val theParameter = fullName2id("p.A.mUserViaParameter(A).a")


      val mUserViaThisDef = fullName2id("p.A.mUserViaThis().Definition")
      val mUserViaParameterDef = fullName2id("p.A.mUserViaParameter(A).Definition")

      //methodUse
      assert( graph.uses(mUserViaThisDef, methM) )
      assert( ! graph.uses(mUserViaThis, methM) )

      //typeUse
      assert( graph.uses(clazz, clazz) )

      assert( graph.uses(mUserViaParameterDef, methM) )
      assert( !graph.uses(mUserViaParameter, methM) )

      assert( graph.uses(theParameter, clazz) )

    }
  }

  scenario("super use explicit") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A { void m(){} }
        |
        |class B extends A {
        |    void mUserViaSuper(){ super.m(); }
        |}
      """
    ){
      val methM = fullName2id("p.A.m()")
      val mUserViaSuperDef = fullName2id("p.B.mUserViaSuper().Definition")

      assert( graph.uses(mUserViaSuperDef, methM) )
    }
  }

  scenario("super use implicit") {
    val _ = new ScenarioFactory(
      """package p;
        |
        |class A { void m(){} }
        |
        |class B extends A {
        |    void mUserViaSuper(){ m(); }
        |}
      """){
      val methM = fullName2id("p.A.m()")
      val mUserViaSuperDef = fullName2id("p.B.mUserViaSuper().Definition")

      assert( graph.uses(mUserViaSuperDef, methM) )
    }
  }



  scenario("field type use") {
    val s = new ScenarioFactory(
    """package p;
      |
      |interface I {}
      |
      |class A { I field; }
    """)
    import s._

    assert( graph.uses("p.A.field", "p.I") )

  }

  scenario("method return type use") {
    val s = new ScenarioFactory(
    """package p;
      |
      |class A { A get(){return this;} }
    """)

    import s._

    assert( graph.uses("p.A.get()", "p.A") )
    assert( graph.typ("p.A.get()") == NamedType("p.A") )
    assert( ! (graph.usesListExludingTypeUses contains (("p.A.get()", "p.A") : NodeIdP) ))

  }

}
