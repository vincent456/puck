package puck

import org.scalatest.Assertions
import puck.graph._

import scalaz.{\/-, -\/}


trait TryValues {
  self : Assertions =>



  implicit class TriedValue[G]( t : Try[G]) /*extends AnyVal*/ {
    def value : G = t match {
      case -\/(_) => assert(false)
        sys.error("false asserted")
      case \/-(g) => g
    }

    def error : PuckError = t match {
      case -\/(err) => err
      case \/-(_) => assert(false)
        sys.error("false asserted")
    }
  }

  def assertSuccess[G](t : Try[G])(f : G => Unit) : Unit = f(t.value)
  def assertIsFailure(t : Try[Any]) : Unit = { val _ = t.error }
}
