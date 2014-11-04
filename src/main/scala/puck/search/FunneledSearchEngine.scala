package puck.search
import scala.collection.mutable

/**
 * Created by lorilan on 26/10/14.
 */

trait Evaluator[Result]{
  def evaluate(r : SearchState[Result]) : Double
}


/*

trait FunneledSeachEngine[Result] extends StackedSearchEngine[Result]{
  this : Evaluator[Result] =>


  val funneledStates = new FunneledStates(30, this)

  def markPointPeriod : Int = 5

  var currentMileStone = markPointPeriod

  override def search() = {
    init()

    def searchIteration(){
      println(exploredStates)

      stateStack pushAll funneledStates()
      funneledStates.clear()
      currentMileStone += markPointPeriod

      while (stateStack.nonEmpty) {
        if (stateStack.head.triedAll)
          stateStack.pop()
        else if (stateStack.head.markedPointDepth == currentMileStone)
          funneledStates += stateStack.pop()
        else {
          stateStack.head.executeNextChoice()
        }
      }
      if(funneledStates().nonEmpty)
        searchIteration()
    }

    searchIteration()
  }


}

class FunneledStates[Result](val sizeMax : Int,
                             val evaluator : Evaluator[Result]) {

  private [this] val content = mutable.Queue[SearchState[Result, _]]()

  val r = new scala.util.Random()

  def apply() : Iterable[SearchState[Result, _]] = content

  def clear() = {
    content.clear()
    acceptanceThreshold = Int.MinValue
    numStates = 0
  }

  private [this] var numStates = 0

  private [this] var acceptanceThreshold = Double.MinValue

  private [this] val worseCaseAcceptanceProbability = 0.2

  def acceptanceCondition() : Boolean = {
    r.nextDouble()<= 0.2
  }


  def +=(s : SearchState[Result, _]){

    def willBeAdd() : Boolean = {
      val value = evaluator.evaluate(s)
      if(value >= acceptanceThreshold){
        acceptanceThreshold = value
        true
      }
      else if (numStates < sizeMax)
        true
      else
        acceptanceCondition()
    }

    if(willBeAdd()) {
      if (numStates >= sizeMax) {
        content.dequeue()
      }
      content.enqueue(s)
      numStates += 1
    }
  }

}
*/
