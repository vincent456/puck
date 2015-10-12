package puck.search


import puck.util.Logged

trait Evaluator[T]{
  
  type StateT = SearchState[T]

  def filterDifferentStates (l : Seq[StateT]) : Seq[StateT] = {
    def aux(l : Seq[StateT], acc : Seq[StateT]) : Seq[StateT] = {
      if (l.nonEmpty) {
        aux(l.tail,
          if (!l.tail.exists { st => equals(st, l.head)})
            l.head +: acc
          else acc)
      }
      else acc
    }
    aux(l, Seq())
  }

  def filterDifferentStates (m : Map[Int, Seq[StateT]]) : Map[Int, Seq[StateT]] =
    m.foldLeft(Map[Int, Seq[StateT]]()){
      case (acc, (k, v)) => acc + (k -> filterDifferentStates(v))
    }

  def sort(l : Seq[StateT], precision : Int = 2)={
    def aux(acc : Map[Int, Seq[StateT]], l : Seq[StateT]) :
    Map[Int, Seq[StateT]] =
      if(l.isEmpty) acc
      else{
        val presDix = Math.pow(10, precision.toDouble)
        val value = (evaluate(l.head) * presDix).toInt
        val olds = acc.getOrElse(value, Seq())
        aux(acc + (value -> (l.head +: olds)), l.tail)
      }
    aux(Map[Int, Seq[StateT]](), l)
  }

  def evaluate(r : StateT) : Double
  def equals(r1 : StateT, r2 : StateT) : Boolean

  def sortedDifferentStates(l : Seq[StateT]) :  Map[Int, Seq[StateT]] =
    filterDifferentStates(sort(l))
  
}

class SearchStrategyWithEvaluator[T]
( val evaluator : Evaluator[T]
  ) extends SearchStrategy[T]{

  def continue(se : SearchEngine[T]) : Boolean =
    remainingStates.nonEmpty

  lazy val funneledStates = new StatesStackWithEvalutor[T](evaluator)

  def markPointPeriod : Int = 2

  var currentCheckPoint : Int = markPointPeriod



  override def addState[S <: StateCreator[T, S]](cr : Logged[T], choices : S): Unit ={
    val newState = remainingStates.head.createNextState[S](cr, choices)

    if ( newState.markedPointDepth >= currentCheckPoint)
      funneledStates push newState
    else remainingStates add newState

  }

  var i = 0
  override def oneStep(se : SearchEngine[T]) : Unit = {

    while (remainingStates.nonEmpty) {
      super.oneStep(se)
    }

    i += 1
    println(s"preparing iteration $i ...")

    val freezedStates = funneledStates.produce()

    remainingStates.removeAll()
    remainingStates addAll freezedStates
    funneledStates.clear()

    currentCheckPoint += markPointPeriod
  }

}

class StatesStackWithEvalutor[T](val evaluator : Evaluator[T]) {

  val sizeMax: Int = 10

  private [this] var content = Map[Int, Seq[SearchState[T]]]()

  def produce() : Seq[SearchState[T]] = {
    content = evaluator.filterDifferentStates(content)
    val values = content.keys.toSeq.sorted.reverse

    val (res, _) = values.foldLeft((Seq[SearchState[T]](), 0)){
      case ((acc, size), k) =>
        if(size>=sizeMax) (acc, size)
        else{
          val vs = content(k)
          if(vs.size + size > sizeMax)
            (vs.slice(0, sizeMax - size) ++: acc, sizeMax)
          else (vs ++: acc, size)
        }
    }

    res
  }

  def clear() = {
    content = Map[Int, Seq[SearchState[T]]]()
  }


  def push(s : SearchState[T]): Unit = {
    val value = (evaluator.evaluate(s) * 100).toInt
    val olds = content.getOrElse(value, Seq())
    content =  content + (value -> (s +: olds))
  }

}

