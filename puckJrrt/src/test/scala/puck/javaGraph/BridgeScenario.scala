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

import puck.graph.DependencyGraph
import puck.graph.transformations.rules.CreateTypeMember
import puck.graph._
import puck.javaGraph.nodeKind._
import puck.LoggedEitherValues
import puck.jastadd.ExtendJGraphUtils.Rules
import puck.graph.transformations.Recording

object BridgeScenario {
  val path = getClass.getResource("/bridge/hannemann_simplified").getPath
  def apply() = new BridgeScenario()
}

import BridgeScenario.path

class BridgeScenario private()
  extends ScenarioFactory(
    s"$path/screen/BridgeDemo.java",
    s"$path/screen/Screen.java")
    with LoggedEitherValues {

  var printId = 0

  def introClassMoveMethod
  (g : DependencyGraph, className : String, method : NodeId) : (NodeId, DependencyGraph)= {
    val g0 = g.comment("-- introClassMoveMethod (begin) --")
    val (c, g1) = Rules.intro(g0, className, Class)

    val g2 = g1.addContains("screen", c.id).mileStone
    (c.id, Rules.move.typeMember(g2, List(method), c.id,
      Some(CreateTypeMember(Field))).rvalue.
      comment("-- introClassMoveMethod (end) --"))

  }

  def introClassMoveBothMethodAndMerge
  (g : DependencyGraph,
   className : String,
   printMethod1 : NodeId,
   printMethod2 : NodeId) : (NodeId, DependencyGraph) = {
    val (classNode, g1) = introClassMoveMethod(g, className, printMethod1)

    val g2 = Rules.move.typeMember(g1.mileStone, List(printMethod2), classNode,
      Some(CreateTypeMember(Field))).rvalue

    val g3 = Rules.merge.mergeInto(g2, printMethod2, printMethod1).rvalue
    (classNode, Rules.rename(g3.mileStone, printMethod1, "print"))

  }

  val g0 = graph

  val (starPrinter, g1) = introClassMoveBothMethodAndMerge(g0, "StarPrinter",
    "screen.WelcomeStar.printStar(String)", "screen.InfoStar.printStar(String)")


  val (capitalPrinter, g2) = introClassMoveBothMethodAndMerge(g1, "CapitalPrinter",
    "screen.WelcomeCapital.printCapital(String)", "screen.InfoCapital.printCapital(String)")


  def abstractPrinters(g : DependencyGraph, class1 : NodeId, class2 : NodeId) : DependencyGraph = {
    val (AccessAbstraction(printerInterface, _), g2) =
      Rules.abstracter.createAbstraction(g, g getConcreteNode class1,
        Interface, SupertypeAbstraction).rvalue

    val g3 = Rules.rename(g2.addContains("screen", printerInterface), printerInterface, "Printer")

    Rules.makeSuperType(g3.mileStone, class2, printerInterface)().rvalue
  }

  val g3 = abstractPrinters(g2, starPrinter, capitalPrinter)

  val printerField : NodeId = (g3, "screen.WelcomeCapital.capitalprinter_delegate")
  val pf2 : NodeId = (g3, "screen.InfoCapital.capitalprinter_delegate")
  val pf3 : NodeId =  (g3, "screen.WelcomeStar.starprinter_delegate")
  val pf4 : NodeId  = (g3, "screen.InfoStar.starprinter_delegate")

  val printerInterface : NodeId = (g3, "screen.Printer")

  val g4 =
    List((printerField, capitalPrinter),
      (pf2, capitalPrinter),
      (pf3, starPrinter),
      (pf4, starPrinter)).foldLeft(g3){
      case (g, (fid, tid) ) =>
        Rules.redirection.redirectUsesAndPropagate(g.mileStone, (fid, tid),
          AccessAbstraction(printerInterface, SupertypeAbstraction)).rvalue
    }

  val classes = List[NodeId]("screen.WelcomeStar",
    "screen.WelcomeCapital",
    "screen.InfoStar",
    "screen.InfoCapital")
  val ctors =
    List[NodeId]("screen.WelcomeStar.WelcomeStar()",
      "screen.WelcomeCapital.WelcomeCapital()",
      "screen.InfoStar.InfoStar()",
      "screen.InfoCapital.InfoCapital()")
  val ctorsDef =
    List[NodeId]("screen.WelcomeStar.WelcomeStar().Definition",
      "screen.WelcomeCapital.WelcomeCapital().Definition",
      "screen.InfoStar.InfoStar().Definition",
      "screen.InfoCapital.InfoCapital().Definition")


  val (g5, initializers)= classes.foldRight((g4, List[NodeId]())){
    case (c, (g, acc))=>
      val (init, g1) = Rules.intro.initializer(g.mileStone, c)
      (g1, init :: acc)
  }

  def addFactories(g : DependencyGraph, ctors : List[NodeId]) : (DependencyGraph, List[NodeId]) = {

    val (g1, factories) = ctors.foldRight((g, List[NodeId]())){
      case (ctor, (g, acc)) =>
        val (AccessAbstraction(id, _), g2) =
          Rules.abstracter.createAbstraction(g.mileStone, g.getConcreteNode(ctor),
            StaticMethod, DelegationAbstraction).rvalue
        (g2.addContains("screen.BridgeDemo", id), id :: acc)
    }

    val List(wsFactory, wcFactory, isFactory, icFactory) = factories

    val g2 = g1.mileStone.setName(wsFactory, "createWS")
      .setName(wcFactory, "createWC")
      .setName(isFactory, "createIS")
      .setName(icFactory, "createIC").mileStone

    (g2, factories)
  }


  val (g6, factories) = addFactories(g5, ctors)

  val g7 = (ctors zip ctorsDef zip initializers zip factories).foldLeft(g6){
    case (g, (((ctor, ctorDef), init), fcty)) =>
      Rules.redirection.redirectSourceOfInitUseInFactory(g.mileStone, ctor, ctorDef, init, fcty)
  }

  val g8 = Rules.move.typeMember(g7, List(printerField), "screen.Screen").rvalue.mileStone

  val g9 = List(pf2, pf3, pf4).foldLeft(g8.setName(printerField, "printer")){
    case (g, pf) =>
      val g0 = Rules.move.typeMember(g.mileStone, List(pf), "screen.Screen").rvalue
      Rules.merge.mergeInto(g0, pf, printerField).rvalue
  }
  val main : NodeId = "screen.BridgeDemo.main(String[]).Definition"

  val g10 = (ctors zip factories).foldLeft(g9){
    case (g, (ctor, fty)) =>
      Rules.redirection.redirectUsesAndPropagate(g.mileStone, Uses(main, ctor),
        AccessAbstraction(fty, DelegationAbstraction)).rvalue
  }

  def moveInitAndMerge(g : DependencyGraph,
                       initId : NodeId, initId2 : NodeId,
                       initName : String) : DependencyGraph = {
    val g2 =
      Rules.move.typeMember(g.setName(initId, initName),
        List(initId), "screen.Screen", None).rvalue
    Rules.merge.mergeInto(g2, initId2, initId).rvalue
  }


  val List(initStar, initCapital, initStar2, initCapital2) = initializers

  val g11 = moveInitAndMerge(g10, initStar, initStar2, "initStar")

  val g12 = moveInitAndMerge(g11, initCapital, initCapital2, "initCapital")

  def merge2ClassesAnd2Methods(g : DependencyGraph,
                               name : String,
                               c1 : NodeId, c2 :NodeId,
                               m1 : NodeId, m2 : NodeId) : DependencyGraph = {
    val g2 = Rules.merge.mergeInto(g, c1, c2).rvalue
    val g3 = Rules.merge.mergeInto(g2, m1, m2).rvalue
    Rules.rename(g3, c2, name)
  }

  val g13 = merge2ClassesAnd2Methods(g12,
    "WelcomeScreen",
    "screen.WelcomeStar",
    "screen.WelcomeCapital",
    "screen.WelcomeStar.draw()",
    "screen.WelcomeCapital.draw()")

  val g14 = merge2ClassesAnd2Methods(g13,
    "InfoScreen",
    "screen.InfoStar",
    "screen.InfoCapital",
    "screen.InfoStar.draw()",
    "screen.InfoCapital.draw()")

  //comparison
  // g1 - g7 : true
  // g8+ : false

  def gFinal = g14
}



object BridgeManualRefactoringSpecGenRecord {
  def main(args: Array[String]) : Unit = {
    val bs = BridgeScenario()
    Recording.write(path+".pck", bs.fullName2id, bs.gFinal)
  }
}
