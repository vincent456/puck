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

package puck
package javaGraph

import puck.Settings._
import puck.graph.Try
import puck.graph.comparison.Mapping


class BridgeManualRefactoringSpec extends AcceptanceSpec {

  implicit def tryToEither[T]( g : Try[T]) : Either[PuckError, T] = g.toEither

  scenario("bridge simplified ``manual'' refactoring"){
    val bs = BridgeScenario2()

    val recompiledEx = bs.applyChangeAndMakeExample(bs.gFinal, outDir)

//    import scalaz.syntax.show._
//    import puck.util.Debug.showNodeIndex
//    bs.gFinal.nodesIndex.println
//    recompiledEx.graph.nodesIndex.println
//
//    val mapping = Mapping.create(bs.gFinal, recompiledEx.graph)
//
//    println(Mapping.mapCVM(mapping, bs.gFinal.edges.userMap).toList.sortBy(_._1))
//    println(recompiledEx.graph.edges.userMap.toList.sortBy(_._1))


    //      val ns1 = bs.gFinal.nodesId.map(bs.gFinal.fullName).toSet
    //      val ns2 = recompiledEx.graph.nodesId.map(recompiledEx.graph.fullName).toSet
    //      println("ns1 -- ns2" + (ns1 -- ns2))
    //      println("ns2 -- ns1" + (ns2 -- ns1))
    //
    //    import scalaz.syntax.show._
    //    import puck.util.Debug.{showNodeIndex, showEdgesMap}
    //
    //    bs.gFinal.nodesIndex.println
    //    recompiledEx.graph.nodesIndex.println
    //    bs.gFinal.edges.println
    //    recompiledEx.graph.edges.println



    //        println(bs.gFinal.recording.mkString("\n"))
    //        QuickFrame(bs.graph)
//            QuickFrame(bs.gFinal, "g final")
//            QuickFrame(recompiledEx.graph, "recompiled")


    assert( Mapping.equals(bs.gFinal, recompiledEx.graph) )



  }
}
