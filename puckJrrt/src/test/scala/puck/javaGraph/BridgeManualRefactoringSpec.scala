package puck
package javaGraph

import java.io.{FileWriter, File}

import puck.Settings._
import puck.graph.Try
import puck.graph.comparison.Mapping
import puck.util.PuckLog.{Info, NoSpecialContext}
import puck.util.PuckSystemLogger


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
