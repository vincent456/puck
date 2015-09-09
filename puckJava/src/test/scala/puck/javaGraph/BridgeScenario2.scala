package puck.javaGraph


import java.io.{File, FileReader}

import org.scalatest.{OptionValues, EitherValues}
import puck.Java2dot
import puck.PuckError
import puck.QuickFrame
import puck.Settings
import puck.graph.AccessAbstraction
import puck.graph.DependencyGraph
import puck.graph.Uses
import puck.graph._
import puck.graph.constraints.ConstraintsParser
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.constraints.{ConstraintsParser, SupertypeAbstraction}
import puck.graph.transformations.rules.CreateParameter
import puck.graph.transformations.rules.CreateTypeMember
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember}
import puck.graph._
import puck.javaGraph.JGraphUtils._
import puck.javaGraph.nodeKind.Class
import puck.javaGraph.nodeKind.Field
import puck.javaGraph.nodeKind.Interface
import puck.util.LoggedEither
import puck.util.PuckFileLogger
import puck.util.PuckNoopLogger
import puck.util.{LoggedEither, PuckNoopLogger, PuckFileLogger}
import puck.{QuickFrame, Java2dot, PuckError, Settings}
import puck.javaGraph.nodeKind.{Interface, Field, Class}
import puck.javaGraph.JGraphUtils.{transformationRules => TR}

import scalaz.{-\/, \/-}


object BridgeScenario2 {
  val path = Settings.projectPath + "/test_resources/distrib/bridge/hannemann_simplified/"

  implicit def tryToEither[T](g: Try[T]): Either[PuckError, T] = g.toEither

  def apply() = new BridgeScenario2()
}

class BridgeScenario2 private()
  extends ExampleSample(
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
  val welcomeStarMeth = fullName2id(s"$p.WelcomeStar.draw__void")
  val infoStarMeth = fullName2id(s"$p.InfoStar.draw__void")

  val welcomeCapital = fullName2id(s"$p.WelcomeCapital")
  val infoCapital = fullName2id(s"$p.InfoCapital")
  val welcomeCapitalMeth = fullName2id(s"$p.WelcomeCapital.draw__void")
  val infoCapitalMeth = fullName2id(s"$p.InfoCapital.draw__void")


  val printStar1 = fullName2id(s"$p.WelcomeStar.printStar__String")
  val printStar2 = fullName2id(s"$p.InfoStar.printStar__String")

  val printCapital1 = fullName2id(s"$p.WelcomeCapital.printCapital__String")
  val printCapital2 = fullName2id(s"$p.InfoCapital.printCapital__String")

  var printId = 0

  def introClassMoveMethod
  (g : DependencyGraph, className : String, method : NodeId) : (ConcreteNode, DependencyGraph)= {
    val g0 = g.comment("-- introClassMoveMethod (begin) --")
    val (c, g1) = TR.intro(g0, className, Class)

    val g2 = g1.addContains(screen, c.id)
    (c, TR.move.typeMember(g2, List(method), c.id,
      Some(CreateTypeMember(Field))).value.right.value.
      comment("-- introClassMoveMethod (end) --"))
  }

  def introClassMoveBothMethodAndMerge
  (g : DependencyGraph,
   className : String,
   method1 : NodeId,
   method2 : NodeId) : (ConcreteNode, DependencyGraph) = {
    val (classNode, g1) = introClassMoveMethod(g, className, method1)
    val g2 =
      TR.move.typeMember(g1, List(method2), classNode.id,
        Some(CreateTypeMember(Field))).value.right.value

    (classNode, TR.merge.mergeInto(g2, method2, method1).value.right.value)
  }

    val g0 = graph
    /*.newGraph(constraints =
      ConstraintsParser(fullName2id, new FileReader(BridgeScenario.path + "decouple.pl")))*/

    val (_, g1) = introClassMoveBothMethodAndMerge(g0, "StarPrinter", printStar1, printStar2)


    def gFinal = g1

  //  def intro2classMerge
//  ( g : DependencyGraph, className : String,
//    meth1 : NodeId, meth2 : NodeId) = {
//    val g0 = g.comment("-- intro2classMerge (begin) --")
//
//    val (c1, g1) = introClassMoveMethod(g0, className, meth1)
//
//    val (c2, g2) = introClassMoveMethod(g1, className+"Tmp", meth2)
//    val g3 = TR.mergeInto(g2.comment("-- Merging methods (begin) --"), meth2, meth1).value.right.value
//    (c1, TR.mergeInto(g3.comment("-- Merging methods (end) --"), c2.id, c1.id).value.right.value
//      .comment("-- intro2classMerge (end) --"))
//  }
//
//  def useInterfaceInstead
//  (g : DependencyGraph, clazz : NodeId, interface : NodeId) : DependencyGraph =
//    g.usersOf(clazz).foldLeft(g){ (g0, userId) =>
//      TR.redirection.redirectUsesAndPropagate(g0,
//        Uses(userId, clazz),
//        AccessAbstraction(interface,
//          SupertypeAbstraction)).value.right.value
//    }
//
//  def getDelegate(g : DependencyGraph, clazz : NodeId) =
//    g.content(clazz).find{ id =>
//      val n = g.getConcreteNode(id)
//      n.name.endsWith("_delegate")
//    }.value
//
//
//
//  val (c1, g1) = intro2classMerge(g0, "StarStyle", printStar1, printStar2)
//  val (c2, g2) = intro2classMerge(g1, "CapitalStyle", printCapital1, printCapital2)
//  val g3 =  TR.rename(TR.rename(g2, printStar1, "printStyle"), printCapital1, "printStyle")
//
//  val (AccessAbstraction(i1Id, _), g4) = TR.abstracter.createAbstraction(g3, c1, Interface, SupertypeAbstraction).value.right.value
//  val g5 = g4.addContains(screen, i1Id)
//  val (AccessAbstraction(i2Id, _), g6) = TR.abstracter.createAbstraction(g5, c2, Interface, SupertypeAbstraction).value.right.value
//  val g7 = g6.addContains(screen, i2Id)
//
//  val g8 = TR.rename(TR.mergeInto(g7, i2Id, i1Id).value.right.value, i1Id, "StyleProvider")
//
//  val g9 = useInterfaceInstead(g8, c1.id, i1Id)
//
//  val g10 = useInterfaceInstead(g9, c2.id, i1Id)
//
//
//  val delegate = getDelegate(g10, welcomeStar)
//
//  val g11 =
//  //    TR.move.typeMember(TR.rename(g10, delegate, "styleProvider"), List(delegate), screenClass).value.right.value
//    TR.move.typeMember(TR.rename(g10, delegate, "styleProvider"), List(delegate), screenClass, Some(CreateParameter)) match {
//      case LoggedEither(l, \/-(v)) => v
//      case LoggedEither(l, -\/(e)) =>
//        println(l)
//        throw e
//
//    }
//
//  val g12 = TR.mergeInto(g11, getDelegate(g11, infoStar), delegate).value.right.value
//  val g13 = TR.mergeInto(g12, getDelegate(g12, welcomeCapital), delegate).value.right.value
//  val g14 = TR.mergeInto(g13, getDelegate(g13, infoCapital), delegate).value.right.value
//
//
//  val g15 = TR.mergeInto(TR.rename(g14, welcomeStar, "WelcomeScreen"),
//    welcomeCapitalMeth, welcomeStarMeth).value.right.value
//  val g16 = TR.mergeInto(g15, welcomeCapital, welcomeStar).value.right.value
//
//
//  //QuickFrame(g16)
//
//  val g17 = TR.mergeInto(TR.rename(g16, infoStar, "InfoScreen"),
//    infoCapitalMeth, infoStarMeth).value.right.value
//  val g18 = TR.mergeInto(g17, infoCapital, infoStar).value.right.value
//
//  def gFinal = g18

}

