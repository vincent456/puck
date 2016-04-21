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

import org.scalatest.{EitherValues, FeatureSpec, OptionValues}
import puck.Settings._
import puck.graph.{AccessAbstraction, DependencyGraph, NodeId, ShowDG, Uses}
import puck.graph.comparison.Mapping
import puck.graph.constraints.SupertypeAbstraction
import puck.jastadd.ExtendJGraphUtils.{transformationRules => TR}
import puck.{LoggedEitherValues, Settings}
import puck.javaGraph.nodeKind._

/**
  * Created by Loïc Girault on 14/04/16.
  */
object CompositeScenario {
  val path = Settings.projectPath + "/test_resources/distrib/composite/"
  def apply() : CompositeScenario = new CompositeScenario()
}

import CompositeScenario.path
class CompositeScenario private ()
  extends ScenarioFactory(
  s"$path/FSClient.java",
  s"$path/fileSystem/Directory.java",
  s"$path/fileSystem/File.java")
  with EitherValues
  with OptionValues
  with LoggedEitherValues {

   val g0 = graph

  def abstractFile(g : DependencyGraph) : (NodeId, DependencyGraph) = {
    val (AccessAbstraction(itcId, _), g1) =
      TR.abstracter.createAbstraction(g, g getConcreteNode "fileSystem.File",
        Interface, SupertypeAbstraction).rvalue

      val g2 = g1.addContains("fileSystem", itcId)

    (itcId, TR.rename(g2, itcId, "FSElement"))
  }

  val (fsElement, g1) = abstractFile(g0)

  val g2 = g1 //TR.makeSuperType(g1, "fileSystem.Directory", (g1, "fileSystem.FSElement"))().rvalue
  val g3 = TR.redirection.redirectUsesAndPropagate(g2,
    Uses("fileSystem.Directory.files", "fileSystem.File"),
    AccessAbstraction(fsElement, SupertypeAbstraction)).rvalue


  def gFinal = g3
}

class CompositeManualRefactoringSpec
  extends FeatureSpec {

  scenario("composite ``manual'' refactoring"){
    val bs = CompositeScenario()

//    import ShowDG._
//    import Debug._
//    (bs.gFinal, bs.gFinal.nodesIndex).println

    val recompiledEx = bs.applyChangeAndMakeExample(bs.gFinal, outDir)

    assert( Mapping.equals(bs.gFinal, recompiledEx.graph) )



  }
}
