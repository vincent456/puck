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

import puck.AcceptanceSpec

/**
  * Created by Loïc Girault on 04/04/16.
  */
class ExtendJSpec  extends AcceptanceSpec {

  scenario("collect superTypes"){
    val _ = new ScenarioFactory(
      """package p;
        |class C implements I2{}
        | interface I{}
        | interface I2 extends I{}
      """
    ){
      val i = program.findType("p.C")
      i.supertypestransitive().size() shouldBe (3) // with java.lang.Object
    }
  }

  scenario("collect superTypes (interwined hierarchy)"){
    val _ = new ScenarioFactory(
      """package p;
        | interface I{}
        | interface I2 extends I{}
        | interface J{}
        | interface I3 extends I2, J{}
        | class B {}
        | class A extends B implements I3{}
      """
    ){
      val i = program.findType("p.A")

      i.supertypestransitive().size() shouldBe (6) // with java.lang.Object
    }
  }

  scenario("collec child types"){
    val _ = new ScenarioFactory(
      """package p;
        | interface I{}
        | interface I2 extends I{}
        | interface I3 extends I{}
      """
    ){
      val i = program.findType("p.I")
      i.childTypes().size() shouldBe (2)
    }
  }
  scenario("super gen interfaces"){
    val _ = new ScenarioFactory(
      """package p;
        | interface I<T>{}
        | interface I2 extends I<String>{}
      """
    ){
      val i = program.findType("p.I2")
      i.superInterfaces().size() shouldBe (1)
    }
  }



  scenario("collec child gen types"){
    val _ = new ScenarioFactory(
      """package p;
        | interface I<T>{}
        | interface I2 extends I<String>{}
        | interface I3 extends I<Integer>{}
        | interface I4 extends I{}
      """
    ){
      val i = program.findType("p.I")
      i.childTypes().size() shouldBe (3)
    }
  }

  scenario("collec child gen types (sup in library)"){
    val _ = new ScenarioFactory(
      """package p;
        | import java.util.List;
        | interface I extends List<String>{}
        | interface I2 extends List<Integer>{}
        | interface I4 extends List{}
      """
    ){
      val i = program.findType("java.util.List")
      i.childTypes().size() shouldBe (3)
    }
  }

  scenario("from source vs not from source"){
    val _ = new ScenarioFactory(
      """package p;
        | import java.util.List;
        | interface I extends List<String>{}
      """
    ){
      val l = program.findType("java.util.List")
      val i = program.findType("p.I")
      assert(!l.fromSource())
      assert(i.fromSource())

    }
  }

  scenario("pretty print of method with label"){
    val s = new ScenarioFactory(
      """package p;
        | public class C {
        |   public int m(){
        |     loops:
        |     for(int i=0; i <10; i++){
        |         for(int j=0; j <10; i++){
        |             if(i + j == 5)
        |                break loops;
        |         }
        |     }
        |     return 42;
        |   }
        | }""")
      import s._
      program.prettyPrint(System.out)

  }
}
