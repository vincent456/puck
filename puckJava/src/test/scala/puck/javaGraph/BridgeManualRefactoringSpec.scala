package puck
package javaGraph

import java.io.{FileWriter, File}

import puck.graph._
import puck.util.PuckLog.{Info, NoSpecialContext}
import puck.util.PuckSystemLogger


class BridgeManualRefactoringSpec extends AcceptanceSpec {

  implicit def tryToEither[T]( g : Try[T]) : Either[PuckError, T] = g.toEither

  scenario("bridge simplified ``manual'' refactoring"){
      try {
        val bs = BridgeScenario()
//        bs.printDot(bs.gFinal)
//        bs.printCode(bs.gFinal)
//        bs.gFinal.constraints.printConstraints(bs.gFinal, new PuckSystemLogger(_ => true), (NoSpecialContext, Info))

      } catch {
        case e : Throwable =>
          e.printStackTrace()
          assert(false)
      }
  }
}
