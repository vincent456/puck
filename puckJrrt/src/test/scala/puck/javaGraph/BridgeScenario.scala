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

import java.io.FileReader

import org.scalatest.{OptionValues, EitherValues}
import puck.graph.constraints.{ConstraintsParser, SupertypeAbstraction}
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember}
import puck.graph._
import puck.util.LoggedEither
import puck.{PuckError, Settings}
import puck.javaGraph.nodeKind.{Interface, Field, Class}
import puck.jastadd.ExtendJGraphUtils.{transformationRules => TR}

import scalaz.{-\/, \/-}


object BridgeScenario {
  val path = Settings.projectPath + "/test_resources/distrib/bridge/hannemann_simplified/"

  implicit def tryToEither[T](g: Try[T]): Either[PuckError, T] = g.toEither

  def apply() = new BridgeScenario()
}

class BridgeScenario private()
  extends ScenarioFactory(
    BridgeScenario.path + "screen/BridgeDemo.java",
    BridgeScenario.path + "screen/Screen.java")
  with EitherValues
  with OptionValues {

  import BridgeScenario.tryToEither

  val p = "screen"
  val screen = fullName2id(p)
  val screenClass = fullName2id(s"$p.Screen")

  val welcomeStar = fullName2id(s"$p.WelcomeStar")
  val infoStar = fullName2id(s"$p.InfoStar")
  val welcomeStarMeth = fullName2id(s"$p.WelcomeStar.draw()")
  val infoStarMeth = fullName2id(s"$p.InfoStar.draw()")

  val welcomeCapital = fullName2id(s"$p.WelcomeCapital")
  val infoCapital = fullName2id(s"$p.InfoCapital")
  val welcomeCapitalMeth = fullName2id(s"$p.WelcomeCapital.draw()")
  val infoCapitalMeth = fullName2id(s"$p.InfoCapital.draw()")


  val printStar1 = fullName2id(s"$p.WelcomeStar.printStar(String)")
  val printStar2 = fullName2id(s"$p.InfoStar.printStar(String)")

  val printCapital1 = fullName2id(s"$p.WelcomeCapital.printCapital(String)")
  val printCapital2 = fullName2id(s"$p.InfoCapital.printCapital(String)")

  var printId = 0


  def introClassMoveMethod
  (g : DependencyGraph, className : String, method : NodeId) = {
    val g0 = g.comment("-- introClassMoveMethod (begin) --")
    val (c, g1) = TR.intro(g0, className, Class)
    val g2 = g1.addContains(screen, c.id)
    (c, TR.move.typeMember(g2, List(method), c.id,
      Some(CreateTypeMember(Field))).value.right.value.
      comment("-- introClassMoveMethod (end) --"))
  }

  def intro2classMerge
  ( g : DependencyGraph, className : String,
    meth1 : NodeId, meth2 : NodeId) = {
    val g0 = g.comment("-- intro2classMerge (begin) --")

    val (c1, g1) = introClassMoveMethod(g0, className, meth1)

    val (c2, g2) = introClassMoveMethod(g1, className+"Tmp", meth2)
    val g3 = TR.merge.mergeInto(g2.comment("-- Merging methods (begin) --"), meth2, meth1).value.right.value
    (c1, TR.merge.mergeInto(g3.comment("-- Merging methods (end) --"), c2.id, c1.id).value.right.value
      .comment("-- intro2classMerge (end) --"))
  }

  def useInterfaceInstead
  (g : DependencyGraph, clazz : NodeId, interface : NodeId) : DependencyGraph =
    g.usersOfExcludingTypeUse(clazz).foldLeft(g){ (g0, userId) =>
      TR.redirection.redirectUsesAndPropagate(g0,
        Uses(userId, clazz),
        AccessAbstraction(interface,
        SupertypeAbstraction)).value.right.value
    }

  def getDelegate(g : DependencyGraph, clazz : NodeId) =
    g.content(clazz).find{ id =>
      val n = g.getConcreteNode(id)
      n.name.endsWith("_delegate")
    }.value


  val cm = ConstraintsParser(fullName2id, new FileReader(BridgeScenario.path + "decouple.wld"))


  val g0 = graph.newGraph(constraints = cm)

  val (c1, g1) = intro2classMerge(g0, "StarStyle", printStar1, printStar2)
  val (c2, g2) = intro2classMerge(g1, "CapitalStyle", printCapital1, printCapital2)
  val g3 =  TR.rename(TR.rename(g2, printStar1, "printStyle"), printCapital1, "printStyle")

  val (AccessAbstraction(i1Id, _), g4) = TR.abstracter.createAbstraction(g3, c1, Interface, SupertypeAbstraction).value.right.value
  val g5 = g4.addContains(screen, i1Id)
  val (AccessAbstraction(i2Id, _), g6) = TR.abstracter.createAbstraction(g5, c2, Interface, SupertypeAbstraction).value.right.value
  val g7 = g6.addContains(screen, i2Id)

  val g8 = TR.rename(TR.merge.mergeInto(g7, i2Id, i1Id).value.right.value, i1Id, "StyleProvider")

  val g9 = useInterfaceInstead(g8, c1.id, i1Id)

  val g10 = useInterfaceInstead(g9, c2.id, i1Id)


  val delegate = getDelegate(g10, welcomeStar)

  val g11 =
//    TR.move.typeMember(TR.rename(g10, delegate, "styleProvider"), List(delegate), screenClass).value.right.value
    TR.move.typeMember(TR.rename(g10, delegate, "styleProvider"), List(delegate), screenClass, Some(CreateParameter)) match {
      case LoggedEither(l, \/-(v)) => v
      case LoggedEither(l, -\/(e)) =>
        println(l)
        throw e

    }

  val g12 = TR.merge.mergeInto(g11, getDelegate(g11, infoStar), delegate).value.right.value
  val g13 = TR.merge.mergeInto(g12, getDelegate(g12, welcomeCapital), delegate).value.right.value
  val g14 = TR.merge.mergeInto(g13, getDelegate(g13, infoCapital), delegate).value.right.value


  val g15 = TR.merge.mergeInto(TR.rename(g14, welcomeStar, "WelcomeScreen"),
    welcomeCapitalMeth, welcomeStarMeth).value.right.value
  val g16 = TR.merge.mergeInto(g15, welcomeCapital, welcomeStar).value.right.value


  //QuickFrame(g16)

  val g17 = TR.merge.mergeInto(TR.rename(g16, infoStar, "InfoScreen"),
    infoCapitalMeth, infoStarMeth).value.right.value
  val g18 = TR.merge.mergeInto(g17, infoCapital, infoStar).value.right.value

  def gFinal = g18

}
