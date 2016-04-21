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




import java.io.FileWriter

import org.scalatest.{EitherValues, FeatureSpec, OptionValues}
import puck.graph.AccessAbstraction
import puck.graph.DependencyGraph
import puck.graph.constraints._
import puck.graph.transformations.rules.CreateTypeMember
import puck.graph._
import puck.javaGraph.nodeKind._
import puck.{LoggedEitherValues, Settings}
import puck.jastadd.ExtendJGraphUtils.{transformationRules => TR}
import DependencyGraph.findElementByName
import puck.Settings._
import puck.graph.comparison.Mapping

object BridgeScenario {

  val path = Settings.projectPath + "/test_resources/distrib/bridge/hannemann_simplified/"

  def apply() = new BridgeScenario()
}

import BridgeScenario.path

class BridgeScenario private()
  extends ScenarioFactory(
    s"$path/screen/BridgeDemo.java",
    s"$path/screen/Screen.java")
    with EitherValues
    with OptionValues
    with LoggedEitherValues {

  val screen = fullName2id("screen")
  val `screen.Screen` = fullName2id("screen.Screen")

  val `screen.WelcomeStar` = fullName2id("screen.WelcomeStar")
  val `screen.WelcomeStar.WelcomeStar()` = fullName2id("screen.WelcomeStar.WelcomeStar()")
  val `screen.WelcomeStar.WelcomeStar().Definition` = fullName2id("screen.WelcomeStar.WelcomeStar().Definition")
  val `screen.WelcomeStar.draw()` = fullName2id("screen.WelcomeStar.draw()")

  val `screen.InfoStar` = fullName2id("screen.InfoStar")
  val `screen.InfoStar.InfoStar()` = fullName2id("screen.InfoStar.InfoStar()")
  val `screen.InfoStar.InfoStar().Definition` = fullName2id("screen.InfoStar.InfoStar().Definition")
  val `screen.InfoStar.draw()` = fullName2id("screen.InfoStar.draw()")

  val `screen.WelcomeCapital` = fullName2id("screen.WelcomeCapital")
  val `screen.WelcomeCapital.WelcomeCapital()` = fullName2id("screen.WelcomeCapital.WelcomeCapital()")
  val `screen.WelcomeCapital.WelcomeCapital().Definition` = fullName2id("screen.WelcomeCapital.WelcomeCapital().Definition")
  val `screen.WelcomeCapital.draw()` = fullName2id("screen.WelcomeCapital.draw()")

  val `screen.InfoCapital` = fullName2id("screen.InfoCapital")
  val `screen.InfoCapital.InfoCapital()` = fullName2id("screen.InfoCapital.InfoCapital()")
  val `screen.InfoCapital.InfoCapital().Definition` = fullName2id("screen.InfoCapital.InfoCapital().Definition")
  val `screen.InfoCapital.draw()` = fullName2id("screen.InfoCapital.draw()")


  val `screen.WelcomeStar.printStar(String)` = fullName2id("screen.WelcomeStar.printStar(String)")
  val `screen.InfoStar.printStar(String)` = fullName2id("screen.InfoStar.printStar(String)")

  val `screen.WelcomeCapital.printCapital(String)` = fullName2id("screen.WelcomeCapital.printCapital(String)")
  val `screen.InfoCapital.printCapital(String)` = fullName2id("screen.InfoCapital.printCapital(String)")

  var printId = 0

  def introClassMoveMethod
  (g : DependencyGraph, className : String, method : NodeId) : (NodeId, DependencyGraph)= {
    val g0 = g.comment("-- introClassMoveMethod (begin) --")
    val (c, g1) = TR.intro(g0, className, Class)

    val g2 = g1.addContains(screen, c.id)
    (c.id, TR.move.typeMember(g2, List(method), c.id,
      Some(CreateTypeMember(Field))).rvalue.
      comment("-- introClassMoveMethod (end) --"))
  }

  def introClassMoveBothMethodAndMerge
  (g : DependencyGraph,
   className : String,
   printMethod1 : NodeId,
   printMethod2 : NodeId) : (NodeId, DependencyGraph) = {
    val (classNode, g1) = introClassMoveMethod(g, className, printMethod1)

    val g2 = TR.move.typeMember(g1, List(printMethod2), classNode,
      Some(CreateTypeMember(Field))).rvalue

    val g3 = TR.merge.mergeInto(g2, printMethod2, printMethod1).rvalue
    (classNode, TR.rename(g3, printMethod1, "print"))
  }

  val g0 = graph
  /*.newGraph(constraints =
    ConstraintsParser(fullName2id, new FileReader(BridgeScenario.path + "decouple.pl")))*/

  val (starPrinter, g1) = introClassMoveBothMethodAndMerge(g0, "StarPrinter",
    `screen.WelcomeStar.printStar(String)`, `screen.InfoStar.printStar(String)`)
  val (capitalPrinter, g2) = introClassMoveBothMethodAndMerge(g1, "CapitalPrinter",
    `screen.WelcomeCapital.printCapital(String)`, `screen.InfoCapital.printCapital(String)`)


  def abstractPrinters(g : DependencyGraph, class1 : NodeId, class2 : NodeId) : DependencyGraph = {
    val (AccessAbstraction(printerInterface, _), g2) =
      TR.abstracter.createAbstraction(g, g getConcreteNode class1,
        Interface, SupertypeAbstraction).rvalue

    val g3 = TR.rename(g2.addContains(screen, printerInterface), printerInterface, "Printer")

//    val printerMethod = findElementByName(g3, s"screen.Printer.$method1LocalName").value.id
//    val g4 = TR.rename(TR.rename(g3, printerMethod, "print"), method2, "print")

    TR.makeSuperType(g3, class2, printerInterface)().rvalue
  }

  val g3 = abstractPrinters(g2, starPrinter, capitalPrinter)

  val printerField = findElementByName(g3,
    "screen.WelcomeCapital.capitalprinter_delegate").value.id

  val pf2 = findElementByName(g3,
    "screen.InfoCapital.capitalprinter_delegate").value.id

  val pf3 = findElementByName(g3,
    "screen.WelcomeStar.starprinter_delegate").value.id
  val pf4 = findElementByName(g3,
    "screen.InfoStar.starprinter_delegate").value.id

  val printerInterface = findElementByName(g3, "screen.Printer").value.id

  val g4 =
    List((printerField, capitalPrinter),
        (pf2, capitalPrinter),
        (pf3, starPrinter),
        (pf4, starPrinter)).foldLeft(g3){
      case (g, (fid, tid) ) =>
        TR.redirection.redirectUsesAndPropagate(g, g.getUsesEdge(fid, tid).value,
          AccessAbstraction(printerInterface, SupertypeAbstraction)).rvalue
    }

  val classes = List(`screen.WelcomeStar`,
    `screen.WelcomeCapital`,
    `screen.InfoStar`,
    `screen.InfoCapital`)
  val ctors =
    List(`screen.WelcomeStar.WelcomeStar()`,
      `screen.WelcomeCapital.WelcomeCapital()`,
      `screen.InfoStar.InfoStar()`,
      `screen.InfoCapital.InfoCapital()`)
  val ctorsDef =
    List(`screen.WelcomeStar.WelcomeStar().Definition`,
      `screen.WelcomeCapital.WelcomeCapital().Definition`,
      `screen.InfoStar.InfoStar().Definition`,
      `screen.InfoCapital.InfoCapital().Definition`)

  val (g5, initializers)= classes.foldRight((g4, List[NodeId]())){
    case (c, (g, acc))=>
    val (init, g1) = TR.intro.initializer(g, c)
    (g1, init :: acc)
  }

  def addFactories(g : DependencyGraph, ctors : List[NodeId]) : (DependencyGraph, List[NodeId]) = {
    val (g1, factories) = ctors.foldRight((g, List[NodeId]())){
      case (ctor, (g, acc)) =>
        val (AccessAbstraction(id, _), g2) =
          TR.abstracter.createAbstraction(g, g.getConcreteNode(ctor),
            StaticMethod, DelegationAbstraction).rvalue
        (g2, id :: acc)
    }

    val List(wsFactory, wcFactory, isFactory, icFactory) = factories

      val g2 = g1.setName(wsFactory, "createWS")
            .setName(wcFactory, "createWC")
            .setName(isFactory, "createIS")
            .setName(icFactory, "createIC")

      val `screen.BridgeDemo` = fullName2id("screen.BridgeDemo")

    (factories.foldLeft(g2)(_.addContains(`screen.BridgeDemo`, _)), factories)
  }


  val (g6, factories) = addFactories(g5, ctors)

  val g7 = (ctors zip ctorsDef zip initializers zip factories).foldLeft(g6){
    case (g, (((ctor, ctorDef), init), fcty)) =>
      TR.redirection.redirectSourceOfInitUseInFactory(g, ctor, ctorDef, init, fcty)
  }

  val g8 = TR.move.typeMember(g7, List(printerField), `screen.Screen`).rvalue
  val g9 = List(pf2, pf3, pf4).foldLeft(g8.setName(printerField, "printer")){
    case (g, pf) => TR.merge.mergeInto(g, pf, printerField).rvalue
  }

  val main = fullName2id("screen.BridgeDemo.main(String[]).Definition")

  val g10 = (ctors zip factories).foldLeft(g9){
    case (g, (ctor, fty)) =>
      TR.redirection.redirectUsesAndPropagate(g, Uses(main, ctor),
        AccessAbstraction(fty, DelegationAbstraction)).rvalue
  }

  def moveInitAndMerge(g : DependencyGraph,
                       initId : NodeId, initId2 : NodeId,
                       initName : String) : DependencyGraph = {
    val g2 =
      TR.move.typeMember(g.setName(initId, initName),
        List(initId),`screen.Screen`, None).rvalue
    TR.merge.mergeInto(g2, initId2, initId).rvalue
  }


    val List(initStar, initCapital, initStar2, initCapital2) = initializers

  val g11 = moveInitAndMerge(g10, initStar, initStar2, "initStar")

  val g12 = moveInitAndMerge(g11, initCapital, initCapital2, "initCapital")

  def merge2ClassesAnd2Methods(g : DependencyGraph,
                               name : String,
                               c1 : NodeId, c2 :NodeId,
                               m1 : NodeId, m2 : NodeId) : DependencyGraph = {
    val g2 = TR.merge.mergeInto(g, c1, c2).rvalue
    val g3 = TR.merge.mergeInto(g2, m1, m2).rvalue
    TR.rename(g3, c2, name)
  }

  val g13 = merge2ClassesAnd2Methods(g12,
    "WelcomeScreen",
    `screen.WelcomeStar`,
    `screen.WelcomeCapital`,
    `screen.WelcomeStar.draw()`,
    `screen.WelcomeCapital.draw()`)

  val g14 = merge2ClassesAnd2Methods(g13,
    "InfoScreen",
    `screen.InfoStar`,
    `screen.InfoCapital`,
    `screen.InfoStar.draw()`,
    `screen.InfoCapital.draw()`)

  //comparison
  // g1 - g4 : true
  // g5 - g6 : false
  // g7 : true
  // g8+ : false

  def gFinal = g14
}

class BridgeManualRefactoringSpec extends FeatureSpec {


  scenario("bridge ``manual'' refactoring"){
    val bs = BridgeScenario()

    //QuickFrame(bs.gFinal, "BS", JavaDotHelper)

    val recompiledEx = bs.applyChangeAndMakeExample(bs.gFinal, outDir)

    val g = bs.gFinal
    val g2 =  recompiledEx.graph
//    import puck.util.Debug._
//    import ShowDG._
//
//    val gwriter = new FileWriter("/tmp/nodes.g")
//    gwriter.write((g, g.nodesIndex).shows)
//    gwriter.close()
//
//    val g2writer = new FileWriter("/tmp/nodes.g2")
//    g2writer.write((g2, g2.nodesIndex).shows)
//    g2writer.close()

    assert( Mapping.equals(bs.gFinal, recompiledEx.graph) )



  }
}