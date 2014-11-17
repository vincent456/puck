package puck.util

import scala.util.{Success, Try}

/**
 * Created by lorilan on 15/11/14.
 */
object ErrorHandling {

  def traverse[A, B](a: Iterable[A], b: B)(f: (B, A) => Try[B]): Try[B] =
    a.foldLeft(Success(b): Try[B]){case (b0, a0) =>
      if(b0.isSuccess)f(b0.get, a0)
      else b0
    }
}
