package puck.util

import puck.graph.{LoggedOr, Logged, LoggedOps}

import scalaz._, Scalaz._


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

  def foldEither[A, B, E](a: Set[A], b: B)(f: (B, A) => E \/ B): E \/ B =
    a.foldLeftM[({ type T[x] = E \/ x })#T, B](b)(f)
    //a.foldLeft[E\/B](\/-(b)){case (b0, a0) => b0 flatMap (f(_, a0))}

  def foldLoggedOr[F[_], A, E, B]
  (a: F[A], b: B)
  (f: (B, A) => LoggedOr[E, B])
  (implicit F: Foldable[F]): LoggedOr[E, B] =
    a.foldLeftM[({ type L[x] = LoggedOr[E, x] })#L, B](b)(f)(
      EitherT.eitherTMonad[Logged, E]
    )


  def foldLoggedOr[F[_],A, E, B]
  ( a: F[A], lb: Logged[B])
    ( f: (B, A) => LoggedOr[E, B])
    (implicit F: Foldable[F]):  LoggedOr[E, B] = {
      lb.toLoggedOr.flatMap(foldLoggedOr[F,A,E,B](a,_)(f))
    }

}
