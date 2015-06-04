package puck.util

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

}
