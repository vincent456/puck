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

package puck.javaGraph.graphBuilding

import org.extendj.ast.JavaJastAddDG2AST
import puck.config.Config.SingleFile
import puck.config.Config
import puck.javaGraph.ScenarioFactory
import puck.util.PuckFileLogger
import puck.{Settings, AcceptanceSpec, Project}

/**
  * Created by Loïc Girault on 25/02/16.
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
