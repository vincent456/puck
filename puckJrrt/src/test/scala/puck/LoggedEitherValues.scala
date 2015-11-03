package puck

import org.scalatest.Assertions
import puck.util.LoggedEither

import scalaz.{\/-, -\/}


trait LoggedEitherValues {
  self : Assertions =>

  def assertIsLeft[E,G](t : LoggedEither[E,G]) : Unit = { val _ = t.left }

  implicit class LoggedEitherValue[E, G]( t : LoggedEither[E, G]) /*extends AnyVal*/ {
    def right : G = t.value match {
      case -\/(err) => assert(false, s"right expected, got $err\nlog : ${t.log}")
        sys.error("false asserted")
      case \/-(g) => g
    }

    def left : E = t.value match {
      case -\/(err) => err
      case \/-(r) => assert(false, s"left expected, got $r\nlog : ${t.log}")
        sys.error("false asserted")
    }
  }

}
