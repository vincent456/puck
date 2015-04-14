package puck.javaGraph

import org.scalatest.{OptionValues, EitherValues}
import puck.{PuckError, Settings}
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations.rules.CreateTypeMember
import puck.javaGraph.nodeKind.{Interface, Field, Class}
import puck.javaGraph.transformations.{JavaTransformationRules => TR}


object BridgeScenario {
  val path = Settings.projectPath + "/test_resources/distrib/bridge/hannemann_simplified"

  implicit def tryToEither[T](g: Try[T]): Either[PuckError, T] = g.toEither

  def apply() = new BridgeScenario()
}

class BridgeScenario private()
  extends ExampleSample(
    BridgeScenario.path + "/screen/BridgeDemo.java",
    BridgeScenario.path+"/screen/Screen.java")
  with EitherValues
  with OptionValues {

  import BridgeScenario.tryToEither

  val p = "screen"
  val screen = fullName2id(p)
  val screenClass = fullName2id(s"$p.Screen")

  val welcomeStar = fullName2id(s"$p.WelcomeStar")
  val infoStar = fullName2id(s"$p.InfoStar")
  val welcomeStarMeth = fullName2id(s"$p.WelcomeStar.welcome__void")
  val infoStarMeth = fullName2id(s"$p.InfoStar.info__void")

  val welcomeCapital = fullName2id(s"$p.WelcomeCapital")
  val infoCapital = fullName2id(s"$p.InfoCapital")
  val welcomeCapitalMeth = fullName2id(s"$p.WelcomeCapital.welcome__void")
  val infoCapitalMeth = fullName2id(s"$p.InfoCapital.info__void")


  val starPrint1 = fullName2id(s"$p.WelcomeStar.printStar__String")
  val starPrint2 = fullName2id(s"$p.InfoStar.printStar__String")

  val capitalPrint1 = fullName2id(s"$p.WelcomeCapital.printCapital__String")
  val capitalPrint2 = fullName2id(s"$p.InfoCapital.printCapital__String")




  def introClassMoveMethod
  (g : DependencyGraph, className : String, method : NodeId) = {
    val (c, g1) = TR.intro.createNode(g, className, Class, None)
    val g2 = g1.addContains(screen, c.id)
    (c, TR.move.typeMember(g2, method, c.id,
      CreateTypeMember(Field)).right.value)
  }

  def intro2classMerge
  ( g : DependencyGraph, className : String,
    meth1 : NodeId, meth2 : NodeId) = {
    val (c1, g1) = introClassMoveMethod(g, className, meth1)
    val (c2, g2) = introClassMoveMethod(g1, className+"Tmp", meth2)
    val g3 = TR.mergeInto(g2, meth2, meth1).right.value

    (c1, TR.mergeInto(g3, c2.id, c1.id).right.value)
  }

  def useInterfaceInstead
  (g : DependencyGraph, clazz : NodeId, interface : NodeId) : DependencyGraph =
    g.usersOf(clazz).foldLeft(g){ (g0, userId) =>
      TR.redirection.redirectUsesAndPropagate(g0,
        DGEdge.uses(userId, clazz),
        interface,
        SupertypeAbstraction).right.value
    }

  def getDelegate(g : DependencyGraph, clazz : NodeId) =
    g.content(clazz).find{ id =>
      val n = g.getConcreteNode(id)
      n.name.endsWith("_delegate")
    }.value



  val g0 = graph
  val (c1, g1) = intro2classMerge(g0, "StarPrinter", starPrint1, starPrint2)
  val (c2, g2) = intro2classMerge(g1, "CapitalPrinter", capitalPrint1, capitalPrint2)
  val g3 = g2.setName(starPrint1, "print").setName(capitalPrint1, "print")

  val (i1, g4) = TR.createAbstraction(g3, c1, Interface, SupertypeAbstraction).right.value
  val g5 = g4.addContains(screen, i1.id)
  val (i2, g6) = TR.createAbstraction(g5, c2, Interface, SupertypeAbstraction).right.value
  val g7 = g6.addContains(screen, i2.id)

  val g8 = TR.mergeInto(g7, i2.id, i1.id).right.value

  val g9 = useInterfaceInstead(g8.setName(i1.id, "Printer"), c1.id, i1.id)

  val g10 = useInterfaceInstead(g9, c2.id, i1.id)

  val delegate = getDelegate(g10, welcomeStar)

  val g11 = TR.move.typeMember(g10.setName(delegate, "printer"), delegate, screenClass).right.value
  val g12 = TR.mergeInto(g11, getDelegate(g11, infoStar), delegate).right.value
  val g13 = TR.mergeInto(g12, getDelegate(g12, welcomeCapital), delegate).right.value
  val g14 = TR.mergeInto(g13, getDelegate(g13, infoCapital), delegate).right.value


  val g15 = TR.mergeInto(g14.setName(welcomeStar, "WelcomeScreen"),
    welcomeCapitalMeth, welcomeStarMeth).right.value
  val g16 = TR.mergeInto(g15, welcomeCapital, welcomeStar).right.value

  val g17 = TR.mergeInto(g16.setName(infoStar, "InfoScreen"),
    infoCapitalMeth, infoStarMeth).right.value
  val g18 = TR.mergeInto(g17, infoCapital, infoStar).right.value

  def gFinal = g18

}
