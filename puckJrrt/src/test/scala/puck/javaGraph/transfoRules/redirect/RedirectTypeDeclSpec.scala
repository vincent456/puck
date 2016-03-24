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

import puck.graph._
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Redirection}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind._
import puck.{AcceptanceSpec, Settings}

//import scalaz.syntax.show._
//import puck.util.Debug.showNodeIndex
//println(graph.nodesIndex.shows)




class RedirectTypeDeclSpec
  extends AcceptanceSpec {

  val examplesPath = s"${Settings.testExamplesPath}/redirection/typeDecl/"

  scenario("From class to superType interface") {
    val _ = new ScenarioFactory(s"$examplesPath/ClassToInterfaceSuperType.java") {
      val mUserDecl = fullName2id("p.A.mUser(ClassUsed)")
      val theParam = fullName2id("p.A.mUser(ClassUsed).cu")
      val mUserDef = fullName2id("p.A.mUser(ClassUsed).Definition")

      val classUsed = fullName2id("p.ClassUsed")
      val mUsed = fullName2id("p.ClassUsed.mUsed()")
      val superType = fullName2id("p.SuperType")
      val absmUsed = fullName2id("p.SuperType.mUsed()")

      val typeUse = Uses(theParam, classUsed)
      assert(typeUse.existsIn(graph))
      assert(Uses(mUserDef, mUsed).existsIn(graph))

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          typeUse, AccessAbstraction(superType, SupertypeAbstraction)).right

      assert(Uses(theParam, superType).existsIn(g2))
      assert(Uses(mUserDef, absmUsed).existsIn(g2))

    }
  }

  //val classToClassSupertype

  //val interfaceToInterfaceSuperType

  scenario("From class to delegator class") {
    val _ = new ScenarioFactory(s"$examplesPath/ClassToClassDelegate.java") {
      val mUserDecl = fullName2id("p.A.mUser(Delegatee)")
      val theParam = fullName2id("p.A.mUser(Delegatee).d")

      val mUserDef = fullName2id("p.A.mUser(Delegatee).Definition")

      val delegatee = fullName2id("p.Delegatee")
      val mDelegatee = fullName2id("p.Delegatee.mUsed()")

      val delegator = fullName2id("p.Delegator")
      val mDelegator = fullName2id("p.Delegator.mUsed()")

      val g = graph.addAbstraction(delegatee, AccessAbstraction(delegator, DelegationAbstraction))
        .addAbstraction(mDelegatee, AccessAbstraction(mDelegator, DelegationAbstraction))

      val typeUse = Uses(theParam, delegatee)
      assert(typeUse.existsIn(graph))
      assert(Uses(mUserDef, mDelegatee).existsIn(graph))


      val g2 =
        Redirection.redirectUsesAndPropagate(g,
          typeUse, AccessAbstraction(delegator, DelegationAbstraction)).right

      assert(Uses(theParam, delegator).existsIn(g2))
      assert(Uses(mUserDef, mDelegator).existsIn(g2))
    }

  }

  /*val interfaceToClassDelegate = new ExampleSample(typeDeclPath + "interfaceToClassDelegate/A.java"){
    val rootPackage = fullName2id("interfaceToClassDelegate")
    val mUser = fullName2id("interfaceToClassDelegate.A.mUser__I")
    val interface = fullName2id("interfaceToClassDelegate.I")
    val delegator = fullName2id("interfaceToClassDelegate.Delegator")
  }*/

  scenario("From class to superType interface as type parameter context") {
    val _ = new ScenarioFactory(s"$examplesPath/AsTypeParameterClassToInterface.java") {
      val actualTypeParam = fullName2id("p.A")
      val actualTypeParamMethod = fullName2id("p.A.m()")

      val interfaceTypeParam = fullName2id("p.I")
      val interfaceMethod = fullName2id("p.I.m()")

      val field = fullName2id("p.B.wa")

      val userClass = fullName2id("p.B")
      val userMethodDef = fullName2id("p.B.doM().Definition")

      val genType = fullName2id("p.Wrapper")
      val genericMethod = fullName2id("p.Wrapper.get()")

      val fieldGenTypeUse = graph.getUsesEdge(field, genType).value
      val fieldParameterTypeUse = graph.getUsesEdge(field, actualTypeParam).value
      val typeMemberUse = graph.getUsesEdge(userMethodDef, actualTypeParamMethod).value

      val g2 =
        Redirection.redirectUsesAndPropagate(graph,
          fieldParameterTypeUse, AccessAbstraction(interfaceTypeParam, SupertypeAbstraction)).right

      assert(Uses(field, interfaceTypeParam).existsIn(g2))
      assert(Uses(userMethodDef, interfaceMethod).existsIn(g2))

    }
  }


}
