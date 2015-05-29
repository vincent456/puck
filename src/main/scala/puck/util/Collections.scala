package puck.util

import puck.graph.{LoggedOr, Logged}

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

  def traverse[A, B, E](a: Set[A], b: B)(f: (B, A) => E \/ B): E \/ B =
    a.foldLeftM[({ type T[x] = E \/ x })#T, B](b)(f)
    //a.foldLeft[E\/B](\/-(b)){case (b0, a0) => b0 flatMap (f(_, a0))}

//  def myLift0[E, A]: Logged[A] => EitherT[Logged, E, A] =
//    la => EitherT.right[Logged, E, A](la)

  def foldLog0[A, B, E](a: List[A], b: B)(
    f: (B, A) => LoggedOr[E, B]
    ): LoggedOr[E, B] = a.foldLeftM[({ type L[x] = LoggedOr[E, x] })#L, B](b)(f)(
    EitherT.eitherTMonad[Logged, E]
  )

  def foldLog1[A, B, E]
  ( a: List[A], lb: Logged[B])
  ( f: (B, A) => LoggedOr[E, B]):  LoggedOr[E, B] = {
    val leb = foldLog0(a, lb.value)(f)
    val log  = leb.run.written
    val value = leb.run.value
    EitherT[Logged, E, B]((lb :++> log).map(_ => value))
  }

  def foldLog2[A, B, E]
  ( a: List[A], lb: Logged[B])
  ( f: (B, A) => LoggedOr[E, B]):  Logged[E \/ B] =
    foldLog1(a,lb)(f).run
}
