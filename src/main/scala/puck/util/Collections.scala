package puck.util

import scalaz.{\/-, \/}

/**
 * Created by lorilan on 3/24/15.
 */
object Collections {
  implicit class SelectList[+T](l : List[T]){
    def select(pred : T => Boolean): Option[(T, List[T])] ={
      type Discarded = List[T]
      type Remainings = List[T]

      def aux : (Discarded, Remainings) => Option[(T, List[T])] = {
        case (_, List()) => None
        case (ds, c :: tl) =>
          if(pred(c)) Some((c, ds ::: tl ))
          else aux(c :: ds, tl)
      }
      aux(List(), l)
    }
  }

  //see if it can be rewritten using scalaz !
  def traverse[A, B, E](a: Iterable[A], b: B)(f: (B, A) => E \/ B): E \/ B =
    a.foldLeft[E\/B](\/-(b)){case (b0, a0) => b0 flatMap (f(_, a0))}

}
