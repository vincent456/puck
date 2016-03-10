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

import puck.graph._
import puck.util.{GenericGraphAlgorithms, WeightedDirectedGraph, GreedyCycleRemover}
import puck.util.GreedyCycleRemover.WDGHelper
import puck.{Settings, AcceptanceSpec}

/**
  * Created by Loïc Girault on 08/12/15.
  */
class CycleRemovalTest
  extends AcceptanceSpec {


  val examplesPath = Settings.testExamplesPath + "/cycle"
  feature("Layering algorithm"){
    val rootDir = examplesPath + "/3packages3classes"
    scenario("3 package 3 classes"){
      val scenar = ScenarioFactory.fromDirectory(rootDir)
      import scenar.graph


      val isRelevantNode : (DependencyGraph, NodeId) => Boolean = {
        (g, id) =>
          val n = g.getNode(id)
          n.kind.kindType match {
            case NameSpace | TypeDecl => true
            case _ => false
          }
      }


//      val edges = reduceGraph(graph, isRelevantNode)
//      //graph.usesList ++ graph.typeUsesList
//      val nodes = graph.nodesId.toSet filter (isRelevantNode(graph, _))
//      val remover = new GreedyCycleRemover(NaiveHelper)
//      val edgesToRemove = remover.edgesToRemove((nodes, edges))

      val rg = GenericGraphAlgorithms.reduceGraphG(graph, isRelevantNode)
      //QuickFrame(rg, "RG",JavaDotHelper)
//      val edgesToRemove = greedyCycleRemoval(rg)

      val wdg = WeightedDirectedGraph.fromDG(graph, isRelevantNode)
      val remover = new GreedyCycleRemover(WDGHelper)
      val edgesToRemove = remover.edgesToRemove(wdg).toList

 //      println(edges.mkString(","))
//      println(nodes.mkString(","))
//      println(edgesToRemove)
      assert(edgesToRemove.nonEmpty)
    }

  }

}
