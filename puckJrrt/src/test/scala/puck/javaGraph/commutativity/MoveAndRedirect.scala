package puck.javaGraph.commutativity

import puck.Settings._
import puck.graph.comparison.Mapping
import puck.graph.transformations.rules.{Redirection, Move}
import puck.graph.{LoggedTG, Uses, Factory, AccessAbstraction}
import puck.graph.constraints.DelegationAbstraction
import puck.javaGraph.ScenarioFactory
import puck.{Settings, AcceptanceSpec}

/**
  * Created by lorilan on 04/12/15.
  */
class MoveAndRedirect
  extends AcceptanceSpec {
  val examplesPath = Settings.testExamplesPath + "/redirection/"
  feature("confluence of move and redirect"){
    val typeCtorPath = examplesPath + "typeConstructor"

    scenario("move factory then redirect") {
      val _ = new ScenarioFactory(s"$typeCtorPath/StaticFactory.java") {
        val ctor = fullName2id("p.B.B()")
        val factoryMethod = fullName2id("p.B.createB()")
        val factoryClass = fullName2id("p.Factory")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g =
          graph.addAbstraction(ctor, AccessAbstraction(factoryMethod, DelegationAbstraction))
              .setRole(factoryMethod, Some(Factory(ctor)))

        val ltg : LoggedTG =
        for{
          g0 <- Move.typeMember(g, List(factoryMethod), factoryClass, None)

          gAndUses <- Redirection.redirect(g0, Uses(callerDef, ctor),
            AccessAbstraction(factoryMethod, DelegationAbstraction))
        } yield gAndUses._1

        val g2 = ltg.right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

    scenario("redirect then move factory ") {
      val _ = new ScenarioFactory(s"$typeCtorPath/StaticFactory.java") {
        val ctor = fullName2id("p.B.B()")
        val factoryMethod = fullName2id("p.B.createB()")
        val factoryClass = fullName2id("p.Factory")

        val callerDecl = fullName2id("p.A.m()")
        val callerDef = fullName2id("p.A.m().Definition")

        val g =
          graph.addAbstraction(ctor, AccessAbstraction(factoryMethod, DelegationAbstraction))
            .setRole(factoryMethod, Some(Factory(ctor)))

        val ltg : LoggedTG =
          for{
            gAndUses <- Redirection.redirect(g, Uses(callerDef, ctor),
              AccessAbstraction(factoryMethod, DelegationAbstraction))

            g0 <- Move.typeMember(gAndUses._1, List(factoryMethod), factoryClass, None)
          } yield g0

        val g2 = ltg.right

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert(Mapping.equals(g2, recompiledEx.graph))
      }
    }

  }
}
