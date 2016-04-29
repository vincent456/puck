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

import java.io.{File, FileReader}

import org.scalatest.{EitherValues, FeatureSpec, OptionValues}
import puck.graph.DependencyGraph
import puck.graph.transformations.rules.CreateTypeMember
import puck.graph._
import puck.javaGraph.nodeKind._
import puck.{LoggedEitherValues, QuickFrame}
import puck.jastadd.ExtendJGraphUtils.{transformationRules => Rules}
import puck.Settings._
import puck.graph.comparison.Mapping
import puck.graph.constraints.{ConstraintsParser, DelegationAbstraction, SupertypeAbstraction}
import puck.graph.io.VisibilitySet
import puck.gui.PrintingOptionsControl
import puck.gui.svg.actions.AutoSolveAction
import puck.jastadd.ExtendJGraphUtils
import puck.search.{AStarSearchStrategy, BreadthFirstSearchStrategy, SearchEngine}

import puck.TestUtils._
import scala.swing.{FlowPanel, Panel}
import BridgeScenario.path
import puck.actions.Choose
import puck.graph.constraints.search.{BlindControl, SResultEvaluator}
import puck.util.LoggedEither

import scalaz.\/-

class BridgeAutoSolveUsingGUIS extends FeatureSpec {

  scenario("bridge ``manual'' refactoring"){
    val bs = BridgeScenario()
    //QuickFrame(bs.graph, "Graph", ExtendJGraphUtils.dotHelper)
    import bs._
    val cm = ConstraintsParser(bs.fullName2id, new FileReader(s"$path/decouple.wld"))

    val publisher = new FlowPanel()
    val poc = new PrintingOptionsControl(VisibilitySet.allVisible(bs.graph), publisher)
    poc.typeUsesVisible = true
    new AutoSolveAction(publisher, cm,
      bs.graph getConcreteNode "screen.InfoStar.printStar(String)", poc)(bs.graph, ExtendJGraphUtils).actionPerformed(null)
  }
}

class BridgeAutoSolveSpec extends FeatureSpec {

    scenario("bridge  scenario, auto solve test - BreadthFirstSearchStrategy"){
      val bs = BridgeScenario()
      import bs._
      val cm = ConstraintsParser(bs.fullName2id, new FileReader(s"$path/decouple.wld"))

      val searchControlStrategy =
        new BlindControl(
          Rules,
          bs.graph, cm, bs.graph getConcreteNode "screen.InfoStar.printStar(String)")

      val engine =
        new SearchEngine(
          new BreadthFirstSearchStrategy[(DependencyGraph, Int)],
          searchControlStrategy,
          Some(5)/*,
              evaluator = Some(SResultEvaluator.equalityByMapping(Metrics.nameSpaceCoupling))*/)

      engine.explore()
      println(engine.successes.size + " successes")
      // show successes
      engine.successes foreach showSuccess
      // show successes: alternate version
      //  showEngineSuccesses(engine)


    }

    scenario("bridge  scenario, auto solve test - AStarSearchStrategy"){
      val bs = BridgeScenario()
      import bs._
      val cm = ConstraintsParser(bs.fullName2id, new FileReader(s"$path/decouple.wld"))

      val graphBeforeSearch = bs.graph.mileStone

      val searchControlStrategy =
        new BlindControl(
          Rules,
          graphBeforeSearch, cm, bs.graph getConcreteNode "screen.InfoStar.printStar(String)")

      val engine =
        new SearchEngine(
          new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(Metrics.nameSpaceCoupling)),
          searchControlStrategy,
          Some(5)/*,
              evaluator = Some(SResultEvaluator.equalityByMapping(Metrics.nameSpaceCoupling))*/)

      engine.explore()
      println(engine.successes.size + " successes")

      //engine.successes foreach showSuccess
      engine.successes.zipWithIndex foreach {
        case (ss, i) =>
          val LoggedEither(_, \/-((g, _))) = ss.loggedResult

          var g0 = g
          while (g0.virtualNodes.nonEmpty) {
            QuickFrame(g0, "G"+i, ExtendJGraphUtils.dotHelper)
            val vn = g0.virtualNodes.head
            Choose("Concretize node",
              s"Select a concrete value for the virtual node $vn :",
              vn.potentialMatches.toSeq map g0.getConcreteNode) match {
              case None => ()
              case Some(cn) =>
                import Recording.RecordingOps
                val r2 = g0.recording.subRecordFromLastMilestone.concretize(vn.id, cn.id)
                g0 = r2 redo graphBeforeSearch
            }
          }
          QuickFrame(g0, "G"+i, ExtendJGraphUtils.dotHelper)
          val bs2 : ScenarioFactory = BridgeScenario()
          import puck.util.FileHelper.FileOps
          //puck.ignore(bs2.applyChangeAndMakeExample(g0, Settings.outDir \ ("g"+i)))
          puck.ignore(bs2.applyChanges(g0, new File("/tmp/puckRes"+i)))
      }
      // show successes: alternate version
      //  showEngineSuccesses(engine)

    }
}