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

package puck.util

import scalaz._, Scalaz._

case class LoggedEither[+L, +R](log : String, value : L\/R){

  def :++>(lg : String) : LoggedEither[L, R] = copy(log = log + lg)
  def <++:(lg : String) : LoggedEither[L, R] = copy(log = lg + log)
  def >>[M,S](v : M \/ S) : LoggedEither[M, S] = copy(value = v)
  def right_>>[S](s : S) : LoggedEither[L, S] = copy(value = \/-(s))
  def left_>>[M](m : M): LoggedEither[M, R] = copy(value = -\/(m))

  /** Map on the right of the disjunction. */
  def map[D](g: R => D): LoggedEither[L, D] = copy(value = value map g)

  def flatMap[LL >: L, D](g: R => LoggedEither[LL, D]): LoggedEither[LL, D]=
    this.value match {
      case a @ -\/(_) => this.copy(value = a)
      case \/-(b) =>  this.log <++: g(b)
    }

}

/*sealed abstract class LoggedEither[L, R]{

  val log : String
  val value : L\/R

  def :++>(lg : String) : LoggedEither[L, R] = Return(this.log + lg, this.value)
  def <++:(lg : String) : LoggedEither[L, R] = Return(lg + this.log, this.value)
  //def >>[M<:L, S<:R](a : M\/S) : LoggedEither[M, S] = Return(this.log, a)

  def right_>>[S <: R](s : S) : LoggedEither[L, S] = Return(this.log, \/-(s))
  def left_>>[M <: L](m : M): LoggedEither[M, R] = Return(this.log, -\/(m))


  /** Map on the right of the disjunction. */
  def map[D](g: R => D): LoggedEither[L, D] =
    Return(this.log, this.value map g)
    //flatMap[L, D](a => Return("", \/-(g(a))))

  def flatMap[LL >: L, S](g: R => LoggedEither[LL, S]): LoggedEither[LL, S]=
    Suspend(this, g)



  def valueOr(x: L => R): Logged[R] =
    Writer(log, value valueOr x)
}

case class Return[L, R](log : String, value : L\/R) extends LoggedEither[L, R]
case class Suspend[L, LL>:L, R, S]
( lge0 : LoggedEither[L, R],
  f : R => LoggedEither[LL, S])
  extends LoggedEither[LL, S]{

  lazy val (log, value) = {
    val lge1 : LoggedEither[LL, S] = lge0.value match {
      case a @ -\/(_) => Return(lge0.log, a)
      case \/-(b) => lge0.log <++: f(b)
    }

    (lge1.log, lge1.value)
  }
}*/





object LoggedEither {

  def flatten[A](le : LoggedEither[Error, LoggedEither[Error, A]]): LoggedEither[Error, A] = le.value match {
    case \/-(LoggedEither(log, v)) => LoggedEither(le.log + log, v)
    case -\/(e) => LoggedEither(le.log, -\/(e))
  }

  def apply[L, R]( v : L \/ R):LoggedEither[L,R] =
    LoggedEither("", v)

  def foldEither[A, B, E](a: Set[A], b: B)(f: (B, A) => E \/ B): E \/ B =
    a.foldLeftM[({ type T[x] = E \/ x })#T, B](b)(f)

  implicit class FoldLogSyntax[F[_], A](val a : F[A]) extends AnyVal {

    def foldLoggedEither[E, B]
    ( b : B)
    ( f : (B, A) => LoggedEither[E, B])
    ( implicit F: Foldable[F]): LoggedEither[E, B] =
      foldLoggedEither[E,B](b.set(""))(f)

    def foldLoggedEither[E, B]
    ( lb : Logged[B])
    ( f : (B, A) => LoggedEither[E, B])
    ( implicit F: Foldable[F]):  LoggedEither[E, B] =
      a.foldLeft[LoggedEither[E, B]](LoggedEither(lb.written, lb.value.right[E])) {
        (b0, a0) => b0 flatMap (f(_, a0))
      }

    def foldLoggedEither[E, B]
    ( lb : LoggedEither[E, B])
    ( f : (B, A) => LoggedEither[E, B])
    ( implicit F: Foldable[F]):  LoggedEither[E, B] =
      a.foldLeft[LoggedEither[E, B]](lb) {
        (b0, a0) => b0 flatMap (f(_, a0))
      }
  }

  implicit def loggedEitherMonad[L] =
    new  Monad[({type l[a] = LoggedEither[L, a]})#l] with Cozip[({type l[a] = LoggedEither[L, a]})#l] {

    def bind[A, B](fa: LoggedEither[L, A])(f: A => LoggedEither[L, B]) = fa flatMap f

    def point[A](a: => A) = LoggedEither("", \/-(a))

    def cozip[A, B](a: LoggedEither[L, A \/ B]) : LoggedEither[L, A ] \/ LoggedEither[L, B] =
      a match {

        case LoggedEither(log, -\/(l)) => -\/(LoggedEither(log, -\/(l)))
        case LoggedEither(log, \/-(e)) => e match {
          case -\/(a) => -\/(LoggedEither(log, \/-(a)))
          case \/-(b) => \/-(LoggedEither(log, \/-(b)))
        }
      }
  }
}

