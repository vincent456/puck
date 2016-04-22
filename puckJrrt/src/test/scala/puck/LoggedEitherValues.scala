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

//import org.scalatest.Assertions
import org.scalatest.exceptions.TestFailedException
import puck.graph.DependencyGraph
import puck.util.LoggedEither

import scalaz.{-\/, \/-}


trait LoggedEitherValues {
//  self : Assertions =>
//
//  def assertIsLeft[E,G](t : LoggedEither[E,G]) : Unit = ignore(t.left)
//
//  implicit class LoggedEitherValue[E, G]( t : LoggedEither[E, G]) /*extends AnyVal*/ {
//    def right : G = t.value match {
//      case -\/(err) => assert(false, s"right expected, got $err\nlog : ${t.log}")
//        sys.error("false asserted")
//      case \/-(g) => g
//    }
//
//    def left : E = t.value match {
//      case -\/(err) => err
//      case \/-(r) => assert(false, s"left expected, got $r\nlog : ${t.log}")
//        sys.error("false asserted")
//    }
//  }

  implicit def convertLoggedEitherToValuable[E, G](t : LoggedEither[E, G]) : LoggedEitherValuable[E,G] =
    new LoggedEitherValuable(t, _.toString)

  implicit def convertLoggedTGToValuable(t : LoggedEither[Error, DependencyGraph]) : LoggedEitherValuable[Error, DependencyGraph] =
    new LoggedEitherValuable(t, _.getMessage)

  class LoggedEitherValuable[E, G](t : LoggedEither[E, G], show : E => String) {
    def rvalue : G = t.value match {
      case -\/(err) =>
        throw new TestFailedException(Some("loggedEitherRightValueNotDefined : " + err.toString),
          Some(new PuckError(err.toString)), 0)
      case \/-(g) => g
    }

    def lvalue : E = t.value match {
      case -\/(err) => err
      case \/-(r) =>
        throw new TestFailedException(Some("loggedEitherLeftValueNotDefined"),
          Some(new PuckError()), 0)


    }
  }

}
