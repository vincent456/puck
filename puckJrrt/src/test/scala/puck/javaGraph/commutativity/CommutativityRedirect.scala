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

import puck.javaGraph.nodeKind.Field
import puck.{AcceptanceSpec, Settings}
import puck.graph.comparison.Mapping
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.{Factory, AccessAbstraction, Uses}
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Redirection}
import puck.javaGraph.ScenarioFactory
import puck.Settings.outDir

class CommutativityRedirect
  extends AcceptanceSpec {

  val examplesPath = Settings.testExamplesPath + "/redirection/"

  feature("TypeDecl uses redirection") {

    val typeDeclPath = examplesPath + "typeDecl/"

    scenario("From class to superType interface") {
      val _ = new ScenarioFactory(s"$typeDeclPath/ClassToInterfaceSuperType.java") {
        val theParam = fullName2id("p.A.mUser(ClassUsed).cu")

        val classUsed = fullName2id("p.ClassUsed")
        val superType = fullName2id("p.SuperType")

        val g =
          Redirection.redirectUsesAndPropagate(graph,
            Uses(theParam, classUsed),
            AccessAbstraction(superType, SupertypeAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert(Mapping.equals(g, recompiledEx.graph))

      }
    }

    //val classToClassSupertype

    //val interfaceToInterfaceSuperType

    ignore("From class to delegator class") {
      new ScenarioFactory(s"$typeDeclPath/ClassToClassDelegate.java") {
        val theParam = fullName2id("p.A.mUser(Delegatee).d")

        val delegatee = fullName2id("p.Delegatee")
        val mDelegatee = fullName2id("p.Delegatee.mUsed()")

        val delegator = fullName2id("p.Delegator")
        val mDelegator = fullName2id("p.Delegator.mUsed()")

        //QuickFrame(graph, "g", JavaDotHelper)

        val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
          .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

        val g2 =
          Redirection.redirectUsesAndPropagate(g,
            Uses(theParam, delegatee),
            AccessAbstraction(delegator, DelegationAbstraction)).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      };
      ()

    }

    /*val interfaceToClassDelegate = new ExampleSample(typeDeclPath + "interfaceToClassDelegate/A.java"){
      val rootPackage = fullName2id("interfaceToClassDelegate")
      val mUser = fullName2id("interfaceToClassDelegate.A.mUser__I")
      val interface = fullName2id("interfaceToClassDelegate.I")
      val delegator = fullName2id("interfaceToClassDelegate.Delegator")
    }*/

  }

  feature("TypeConstructor uses redirection") {

    val typeCtorPath = examplesPath + "typeConstructor"

    scenario("From constructor to constructorMethod hosted elsewhere - non static, parameter") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedElsewhere.java") {
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.Factory.createB()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))
                    .setRole(ctorMethod, Some(Factory(ctor)))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    scenario("From constructor to constructorMethod hosted elsewhere - non static, field") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedElsewhere.java") {
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.Factory.createB()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))
                      .setRole(ctorMethod, Some(Factory(ctor)))

        val g2 = Redirection.redirectTypeConstructorToInstanceValueDecl(g,
          Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right


        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    ignore("From constructor to constructorMethod hosted by self - non static, parameter") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java") {
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.B.create()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        println(s"ctor = $ctor")
        println(s"ctorMethod = $ctorMethod")
        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))
                      .setRole(ctorMethod, Some(Factory(ctor)))
        //QuickFrame(g, "g", JavaDotHelper)
        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            Uses(callerDef, ctor),
            AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateParameter).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))

      }
    }

    scenario("From constructor to constructorMethod hosted by self - non static, field") {
      val _ = new ScenarioFactory(s"$typeCtorPath/ConstructorToConstructorMethodHostedBySelf.java") {
        val ctor = fullName2id("p.B.B()")
        val ctorMethod = fullName2id("p.B.create()")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g = graph.addAbstraction(ctor, AccessAbstraction(ctorMethod, DelegationAbstraction))

        val g2 =
          Redirection.redirectTypeConstructorToInstanceValueDecl(g,
            Uses(callerDef, ctor), AccessAbstraction(ctorMethod, DelegationAbstraction))(CreateTypeMember(Field)).right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)
        assert(Mapping.equals(g2, recompiledEx.graph))

      }
    }
  }

  feature("TypeMember uses redirection"){

        val typeMemberPath = examplesPath + "typeMember"

        scenario("From method to method superType"){
          val _ = new ScenarioFactory(s"$typeMemberPath/MethodToMethodSuperType.java") {
            val mUsed = fullName2id("p.Bimpl.m1()")
            val mAbs = fullName2id("p.B.m1()")

            val userDecl = fullName2id("p.A.m()")
            val userDef = fullName2id("p.A.m().Definition")


            val g =
              Redirection.redirectUsesAndPropagate(graph,
                Uses(userDef, mUsed), AccessAbstraction(mAbs, SupertypeAbstraction)).right


            val recompiledEx = applyChangeAndMakeExample(g, outDir)
            assert( Mapping.equals(g, recompiledEx.graph) )


          }
        }

        ignore("From method to method delegate"){

        }

        ignore("From field to ??? delegate"){
          //what should we do ?
        }


  }
}
