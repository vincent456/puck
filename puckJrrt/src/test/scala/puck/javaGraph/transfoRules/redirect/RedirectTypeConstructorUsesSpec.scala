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

import puck.graph.constraints.DelegationAbstraction
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Redirection}
import puck.graph.{AccessAbstraction, Uses}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Field
import puck.{AcceptanceSpec, Settings}

/**
  * Created by Loïc Girault on 24/03/16.
  */
class RedirectTypeConstructorUsesSpec
  extends AcceptanceSpec {

  val examplesPath =  s"${Settings.testExamplesPath}/redirection/typeConstructor"

  scenario("From constructor to constructorMethod hosted elsewhere - non static, parameter") {
    val _ = new ScenarioFactory(s"$examplesPath/ConstructorToConstructorMethodHostedElsewhere.java") {
      val ctor = fullName2id(s"p.B.B()")
      val ctorMethod = fullName2id(s"p.Factory.createB()")
      val factoryClass = fullName2id(s"p.Factory")
      val factoryCtor = fullName2id(s"p.Factory.Factory()")

      val callerDecl = fullName2id(s"p.A.m()")
      val callerDef = fullName2id("p.A.m().Definition")


      val ctorUse = Uses(callerDef, ctor)
      val ctorMethodUse = Uses(callerDef, ctorMethod)

      assert(ctorUse.existsIn(graph))
      assert(!ctorMethodUse.existsIn(graph))

      graph.parametersOf(callerDecl) shouldBe empty

      val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

      val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
        ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right

      assert(ctorMethodUse.existsIn(g2))
      assert(!ctorUse.existsIn(g2))

      val parameters = g2.parametersOf(callerDecl)
      parameters.size shouldBe 1

      assert(g2.uses(parameters.head, factoryClass))
    }
  }

  scenario("From constructor to constructorMethod hosted elsewhere - non static, field") {
    val _ = new ScenarioFactory(s"$examplesPath/ConstructorToConstructorMethodHostedElsewhere.java") {
      val ctor = fullName2id(s"p.B.B()")
      val ctorMethod = fullName2id(s"p.Factory.createB()")
      val factoryClass = fullName2id(s"p.Factory")
      val factoryCtor = fullName2id(s"p.Factory.Factory()")

      val callerDef = fullName2id("p.A.m().Definition")

      val callerHostClass = fullName2id(s"p.A")


      val ctorUse = Uses(callerDef, ctor)
      val ctorMethodUse = Uses(callerDef, ctorMethod)

      assert(ctorUse.existsIn(graph))
      assert(!ctorMethodUse.existsIn(graph))

      val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

      val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
        ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right

      assert(ctorMethodUse.existsIn(g2))
      assert(!ctorUse.existsIn(g2))

      g2.content(callerHostClass).size shouldBe (graph.content(callerHostClass).size + 1)

      //assert(g2.uses(parameters.head, factoryClass))
    }
  }


  scenario("From constructor to constructorMethod hosted by self - non static, parameter") {
    val _ = new ScenarioFactory(s"$examplesPath/ConstructorToConstructorMethodHostedBySelf.java") {
      val ctor = fullName2id(s"p.B.B()")
      val ctorMethod = fullName2id(s"p.B.create()")
      val constructedClass = fullName2id(s"p.B")

      val callerDecl = fullName2id(s"p.A.m()")
      val callerDef = fullName2id("p.A.m().Definition")

      val userOfTheCallerDecl = fullName2id(s"p.C.mc()")
      val userOfTheCallerDef = fullName2id("p.C.mc().Definition")

      val ctorUse = Uses(callerDef, ctor)
      val ctorMethodUse = Uses(callerDef, ctorMethod)

      assert(ctorUse existsIn graph)
      assert(!(ctorMethodUse existsIn graph))

      graph.parametersOf(callerDecl) shouldBe empty

      val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

      val g2 =
        Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right


      assert(!(ctorUse existsIn g2))
      assert(ctorMethodUse existsIn g2)

      val parameters = g2.parametersOf(callerDecl)
      parameters.size shouldBe 1

      assert(g2.uses(parameters.head, constructedClass))

      assert(g2.uses(userOfTheCallerDef, ctor))
      assert(!g2.uses(userOfTheCallerDef, constructedClass))

    }
  }

  scenario("From constructor to constructorMethod hosted by self - non static, field") {
    val _ = new ScenarioFactory(s"$examplesPath/ConstructorToConstructorMethodHostedBySelf.java") {
      val ctor = fullName2id(s"p.B.B()")
      val ctorMethod = fullName2id(s"p.B.create()")
      val constructedClass = fullName2id(s"p.B")

      val callerDecl = fullName2id(s"p.A.m()")
      val callerDef = fullName2id("p.A.m().Definition")

      val userOfTheCallerDecl = fullName2id(s"p.C.mc()")
      val userOfTheCallerDef = fullName2id("p.C.mc().Definition")

      val ctorUse = Uses(callerDef, ctor)
      val ctorMethodUse = Uses(callerDef, ctorMethod)

      val callerHostClass = fullName2id(s"p.A")


      assert(ctorUse existsIn graph)
      assert(!(ctorMethodUse existsIn graph))


      val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

      val g2 =
        Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          ctorUse, AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right


      assert(!(ctorUse existsIn g2))
      assert(ctorMethodUse existsIn g2)

      g2.content(callerHostClass).size shouldBe (graph.content(callerHostClass).size + 1)

      val delegate = g2.nodes.find(n => n.name(g2).endsWith("_delegate")).value

      assert(g2.uses(g2.definitionOf_!(delegate.id), ctor))
      assert(!g2.uses(userOfTheCallerDef, constructedClass))

    }
  }
}
