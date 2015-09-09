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

//      val ns1 = bs.gFinal.nodesId.map(bs.gFinal.fullName).toSet
//      val ns2 = recompiledEx.graph.nodesId.map(recompiledEx.graph.fullName).toSet
//      println(ns1 -- ns2)

//        println(bs.gFinal.recording.mkString("\n"))
//        QuickFrame(bs.graph)
//        QuickFrame(bs.gFinal)
//        QuickFrame(recompiledEx.graph, "recompiled")

        assert( Mapping.equals(bs.gFinal, recompiledEx.graph) )



  }
}
