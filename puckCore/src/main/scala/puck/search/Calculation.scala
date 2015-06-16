//package puck.search
//
//import scalaz.{-\/, \/-, \/}
//
//
//case class Calculation[A, B]
//( choices : Calculation[_, A] \/ A,
//  f : A => B,
//  res : List[B] = List()) {
//
//  def k[C](f: B => C) : Calculation[B, C] =
//    Calculation(-\/(this), f)
//
//}
//
//object FSolver {
//
//  def init[A](a : A) : Calculation[(), A] =
//    Calculation(\/-(a), _ => a, List(a))
//
//  def apply[T]( c : Calculation[_,T]) : List[T] = {
//
//  }
//
//}