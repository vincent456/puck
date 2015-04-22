package puck
package javaGraph

import java.io.{FileWriter, File}

import puck.graph._


class BridgeManualRefactoringSpec extends AcceptanceSpec {

  implicit def tryToEither[T]( g : Try[T]) : Either[PuckError, T] = g.toEither

  scenario("bridge simplified ``manual'' refactoring"){
      try {
        val bs = BridgeScenario()
        bs.printDot(bs.gFinal)
        bs.printCode(bs.gFinal)
      } catch {
        case e : Throwable =>
          e.printStackTrace()
          assert(false)
      }
  }
}
