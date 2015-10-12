package puck.util

import scala.collection.mutable

trait HasChildren[T] {
  def children : Iterable[T]
}

trait BreadthFirstTreeIterator[T <: HasChildren[T]] extends Iterator[T]{

  val root : T

  private val nexts : mutable.Queue[T] = mutable.Queue[T]()

  nexts += root

  override def hasNext : Boolean = nexts.nonEmpty

  override def next() : T = {
    val n = nexts.dequeue()
    nexts ++= n.children
    n
  }
}
