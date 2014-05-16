package puck.graph

import _root_.java.util.NoSuchElementException
import scala.collection.mutable

/**
 * Created by lorilan on 13/05/14.
 */
/*
  implements a width cross (parcours en largeur ?) of the graph via the contains tree
 */
class AGNodeIterator (private var nextOne : AGNode) extends Iterator[AGNode]{

  private val nexts : mutable.Queue[AGNode] = mutable.Queue[AGNode]()

  nexts ++= nextOne.content

  override def hasNext : Boolean = !nexts.isEmpty || !(nextOne == null)

  override def next() : AGNode = {
    val n = nextOne

    try{
      nextOne = nexts.dequeue()
      nexts ++= nextOne.content
    } catch {
      case e : NoSuchElementException => nextOne = null
    }

    n
  }
}
