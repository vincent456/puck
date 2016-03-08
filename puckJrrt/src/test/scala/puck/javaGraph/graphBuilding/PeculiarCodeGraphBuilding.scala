package puck.javaGraph.graphBuilding

import java.io.File

import org.extendj.ast.JavaJastAddDG2AST
import puck.config.Config.SingleFile
import puck.config.{Config, ConfigParser}
import puck.javaGraph.ScenarioFactory
import puck.util.{PuckFileLogger, PuckSystemLogger}
import puck.{Settings, AcceptanceSpec, Project}

/**
  * Created by lorilan on 25/02/16.
  */
class PeculiarCodeGraphBuilding extends AcceptanceSpec {

  val graphBuildingExamplesPath = Settings.testExamplesPath + "/graphBuilding/"

  feature("fixed number of nodes") {

    //implicit val logger = new PuckSystemLogger(_ => true)
    implicit val logger = new PuckFileLogger(_ => true, new java.io.File("/tmp/debugLog"))

    scenario("freemind problem") {
      val cfg = Config.empty put (Config.Keys.srcs, List(SingleFile(s"$graphBuildingExamplesPath/complexHierarchy/View.java")))
      val p = new Project(cfg, JavaJastAddDG2AST)
      val s0 = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]


      val n0Number = s0.initialGraph.nodes.size
      Range(0, 10).foreach { _ =>
        val s = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]
        val nNumber = s.initialGraph.nodes.size

        assert(nNumber == n0Number)
      }
    }

    scenario("freemind problem - uses expected") {
      val _ = new ScenarioFactory(s"$graphBuildingExamplesPath/complexHierarchy/View.java") {

        val getRowCount = fullName2id("p.AttributeTableModel.getRowCount()")
        val startEditingTable = fullName2id("p.View.startEditingTable().Definition")
        val field = fullName2id("p.View.currentAttributeTableModel")

        graph.usedBy(startEditingTable).size should be (2)

        assert(graph.uses(startEditingTable, field))
        assert(graph.uses(startEditingTable, getRowCount))

      }
    }

    scenario("Multi interface with same sig - ref typed as I") {
      val _ = new ScenarioFactory(s"$graphBuildingExamplesPath/complexHierarchy/MultiInterface.java") {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

        val mi = fullName2id("p.Test.mi().Definition")

        assert(graph.uses(mi, im))
        assert(!graph.uses(mi, i2m))
        assert(!graph.uses(mi, cm))

      }
    }

    scenario("Multi interface with same sig - ref typed as I2") {
      val _ = new ScenarioFactory(s"$graphBuildingExamplesPath/complexHierarchy/MultiInterface.java") {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

        val mi2 = fullName2id("p.Test.mi2().Definition")

        assert(graph.uses(mi2, i2m))
        assert(!graph.uses(mi2, im))
        assert(!graph.uses(mi2, cm))

      }
    }

    scenario("Multi interface with same sig - ref typed as AC") {
      val _ = new ScenarioFactory(s"$graphBuildingExamplesPath/complexHierarchy/MultiInterface.java") {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

        val mac = fullName2id("p.Test.mac().Definition")

        assert(graph.uses(mac, im))
        assert(graph.uses(mac, i2m))
        assert(!graph.uses(mac, cm))

      }
    }

    scenario("Multi interface with same sig - ref typed as C") {
      val _ = new ScenarioFactory(s"$graphBuildingExamplesPath/complexHierarchy/MultiInterface.java") {

        val im = fullName2id("p.I.m()")
        val i2m = fullName2id("p.I2.m()")
        val cm = fullName2id("p.C.m()")

         val mc = fullName2id("p.Test.mc().Definition")

        assert(!graph.uses(mc, im))
        assert(!graph.uses(mc, i2m))
        assert(graph.uses(mc, cm))
      }
    }

  }
}
