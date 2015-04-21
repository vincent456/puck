package puck
package javaGraph

import java.io.{FileWriter, File}

import puck.graph._


class BridgeManualRefactoringSpec extends AcceptanceSpec {

  implicit def tryToEither[T]( g : Try[T]) : Either[PuckError, T] = g.toEither

  scenario("bridge simplified ``manual'' refactoring"){
    val bs =  BridgeScenario()

    //import bs._

//    val graphToApply = g1
//
//
//    val jdg2ast = new JavaDG2AST(graphToApply.logger, program, g0, initialRecord, fullName2id, dg2astMap)
//
//    jdg2ast((graphToApply, graphToApply.recording))

//    val fw = new FileWriter(Settings.projectPath + "record.txt")
//    fw.write(bs.fullName2id.toString())
//    bs.gFinal.recording.foreach(t => fw.write(t.toString()+'\n'))
//    fw.close()
  }
}
