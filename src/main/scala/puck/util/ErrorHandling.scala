package puck.util

import scalaz.{Success, Validation}
import scalaz.Validation.FlatMap._

/**
 * Created by lorilan on 15/11/14.
 */
object ErrorHandling {

  def traverse[A, B, E](a: Iterable[A], b: B)(f: (B, A) => Validation[E, B]): Validation[E,B] =
    a.foldLeft(Success(b): Validation[E,B]){case (b0, a0) =>
      if(b0.isSuccess)b0 flatMap (f(_, a0))
      else b0
    }
}
