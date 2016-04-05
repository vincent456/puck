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

import org.extendj.ast.JavaJastAddDG2AST
import puck.config.Config.SingleFile
import puck.config.Config
import puck.graph.Uses
import puck.javaGraph.ScenarioFactory
import puck.util.PuckFileLogger
import puck.{AcceptanceSpec, Project, Settings}

/**
  * Created by Loïc Girault on 25/02/16.
  */
class PeculiarCodeGraphBuilding extends AcceptanceSpec {

  val graphBuildingExamplesPath = Settings.testExamplesPath + "/graphBuilding/"

    //implicit val logger = new PuckSystemLogger(_ => true)
    implicit val logger = new PuckFileLogger(_ => true, new java.io.File("/tmp/debugLog"))

    scenario("Multi interface with same sig - ref typed as I") {
      val _ = new ScenarioFactory(
      """
        |package p;
        |
        |interface I { void m();}
        |interface I2 { void m();}
        |abstract class AC implements I, I2 {}
        |class C extends AC{ public void m(){} }
        |
        |class Test{
        |
        |    I i = new C();
        |    void mi(){ i.m(); }
        |}
      """
      ) {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

        val mi = fullName2id("p.Test.mi().Definition")

        assert(graph.uses(mi, im))
        assert(!graph.uses(mi, i2m))
        assert(!graph.uses(mi, cm))

      }
    }

    scenario("Multi interface with same sig - ref typed as I2") {
      val _ = new ScenarioFactory(
        """
          |package p;
          |
          |interface I { void m();}
          |interface I2 { void m();}
          |abstract class AC implements I, I2 {}
          |class C extends AC{ public void m(){} }
          |
          |class Test{
          |
          |    I2 i2 = new C();
          |    void mi2(){ i2.m(); }
          |}
        """
      ) {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

        val mi2 = fullName2id("p.Test.mi2().Definition")

        assert(graph.uses(mi2, i2m))
        assert(!graph.uses(mi2, im))
        assert(!graph.uses(mi2, cm))

      }
    }

    scenario("Multi interface with same sig - ref typed as AC") {
      val _ = new ScenarioFactory(
        """
          |package p;
          |
          |interface I { void m();}
          |interface I2 { void m();}
          |abstract class AC implements I, I2 {}
          |class C extends AC{ public void m(){} }
          |
          |class Test{
          |    AC ac = new C();
          |    void mac(){ ac.m(); }
          |}
        """
      ) {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

        val mac = fullName2id("p.Test.mac().Definition")

        assert(graph.uses(mac, im))
        assert(graph.uses(mac, i2m))
        assert(!graph.uses(mac, cm))

      }
    }

    scenario("Multi interface with same sig - ref typed as C") {
      val _ = new ScenarioFactory(
        """
          |package p;
          |
          |interface I { void m();}
          |interface I2 { void m();}
          |abstract class AC implements I, I2 {}
          |class C extends AC{ public void m(){} }
          |
          |class Test{
          |    C c = new C();
          |    void mc(){ c.m(); }
          |}
        """
      ) {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

         val mc = fullName2id("p.Test.mc().Definition")

        assert(!graph.uses(mc, im))
        assert(!graph.uses(mc, i2m))
        assert(graph.uses(mc, cm))
      }
    }

    scenario("Use of parameterized method where Type variable instantiated via subclassing"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class ListModel<E>{
          |    E e;
          |    E getE(){ return e;}
          |}
          |class HasElement extends ListModel<String> {}
          |
          |public class C {
          |
          |    HasElement data;
          |
          |    public void m() { data.getE().toString(); }
          |
          |}"""
      ){
        val mDef = fullName2id("p.C.m().Definition")
        val getE = fullName2id("p.ListModel.getE()")

        val hasElement = fullName2id("p.HasElement")
        val string = fullName2id("java.lang.String")
        val toString_ = fullName2id("java.lang.String.toString()")


        assert(graph.uses(mDef, getE))
        assert(graph.uses(mDef, toString_))

        graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, string))
      }
    }


  scenario("Use of parameterized method with RAW Type variable (instantiated) via subclassing"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class ListModel<E>{
        |    E e;
        |    E getE(){ return e;}
        |}
        |class HasElement extends ListModel {}
        |
        |public class C {
        |
        |    HasElement data;
        |
        |    public void m() { data.getE().toString(); }
        |
        |}"""
    ){
      val mDef = fullName2id("p.C.m().Definition")
      val getE = fullName2id("p.ListModel.getE()")

      val hasElement = fullName2id("p.HasElement")
      val obj = fullName2id("java.lang.Object")
      val toString_ = fullName2id("java.lang.Object.toString()")


      assert(graph.uses(mDef, getE))
      assert(graph.uses(mDef, toString_))

      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, obj))
    }
  }

  scenario("Use of parameterized method where Type variable instantiated via interface subtyping"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |interface ListModel<E>{  E getE(); }
        |interface HasElement extends ListModel<String> {}
        |
        |public class C {
        |
        |    HasElement data;
        |
        |    public void m() { data.getE().toString(); }
        |
        |}"""
    ){
      val mDef = fullName2id("p.C.m().Definition")
      val getE = fullName2id("p.ListModel.getE()")

      val hasElement = fullName2id("p.HasElement")
      val string = fullName2id("java.lang.String")
      val toString_ = fullName2id("java.lang.String.toString()")


      assert(graph.uses(mDef, getE))
      assert(graph.uses(mDef, toString_))

      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, string))
    }
  }

  scenario("Use of parameterized method with RAW Type variable (instantiated) via interface subtyping"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |interface ListModel<E>{ E getE(); }
        |interface HasElement extends ListModel {}
        |
        |public class C {
        |
        |    HasElement data;
        |
        |    public void m() { data.getE().toString(); }
        |
        |}"""
    ){
      val mDef = fullName2id("p.C.m().Definition")
      val getE = fullName2id("p.ListModel.getE()")

      val hasElement = fullName2id("p.HasElement")
      val obj = fullName2id("java.lang.Object")
      val toString_ = fullName2id("java.lang.Object.toString()")


      assert(graph.uses(mDef, getE))
      assert(graph.uses(mDef, toString_))

      graph.typeUsesOf(mDef, toString_) should contain (Uses(hasElement, obj))
    }
  }


}
