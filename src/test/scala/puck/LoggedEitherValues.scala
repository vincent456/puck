package puck

import org.scalatest.Assertions
import puck.util.LoggedEither

import scalaz.{\/-, -\/}


trait LoggedEitherValues {
  self : Assertions =>



//  implicit class TriedValue[G]( t : Try[G]) /*extends AnyVal*/ {
//    def value : G = t match {
//      case -\/(err) => assert(false, "success expected, got error : " + err.getMessage)
//        sys.error("false asserted")
//      case \/-(g) => g
//    }
//
//    def error : PuckError = t match {
//      case -\/(err) => err
//      case \/-(_) => assert(false)
//        sys.error("false asserted")
//    }
//  }

//  def assertSuccess[G](t : Try[G])(f : G => Unit) : Unit = f(t.value)
//  def assertIsFailure(t : Try[Any]) : Unit = { val _ = t.error }


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
