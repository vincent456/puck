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

/*
package puck.javaGraph

import puck._
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.javaGraph.Scenarii._
import puck.javaGraph.nodeKind.Interface
import puck.util.PuckSystemLogger

import scalaz.{Success, Failure}

/**
 * Created by Loïc Girault on 3/30/15.
 */
class LoneTest extends AcceptanceSpec {

  def assertSuccess[G](t : Try[G])(f : G => Unit) : Unit = {
    t match {
      case Failure(_) => assert(false)
      case Success(g) => f(g)
    }
  }
  val examplesPath = puck.testExamplesPath + "/intro"

  scenario("Intro interface - no existing super type - field self use in class"){
    val p = "introInterfaceNoExistingSuperTypeWithSelfUse"
    val _ = new ExampleSample(s"$examplesPath/$p/B.java") {
      val classB = fullName2id(s"$p.B")
      val field = fullName2id(s"$p.B.f")

      val fieldUserThatShouldNotBeInInterface =
        fullName2id(s"$p.B.fieldUserThatShouldNotBeInInterface__B")

      assert( graph.directSuperTypes(classB).isEmpty )

      assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
      assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )

      assert( graph.abstractions(classB).isEmpty )
      assert( graph.abstractions(field).isEmpty )
      assert( graph.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty )
      puck.quickFrame(graph)
      assertSuccess(TR.createAbstraction(graph.withLogger(new PuckSystemLogger(_ => true)), graph.getConcreteNode(classB),
        Interface, SupertypeAbstraction)){
        case (itc, g) =>
          assert( g.isa(classB, itc.id) )

          g.abstractions(classB).size shouldBe 1
          assert( g.abstractions(field).isEmpty ,
            "Field cannot be exposed in an interface")
          assert( g.abstractions(fieldUserThatShouldNotBeInInterface).isEmpty,
            "Method use concrete class field via parameter, should not be abstracted")

          assert( graph.uses(fieldUserThatShouldNotBeInInterface, classB) )
          assert( graph.uses(fieldUserThatShouldNotBeInInterface, field) )

      }
    }
  }
}
*/
