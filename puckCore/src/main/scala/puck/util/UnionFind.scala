/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.util

/**
  * Created by Loïc Girault on 08/06/16.
  */
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
/**
  * Union Find implementaion.
  * Find is O(1)
  * Union is O(log(n))
  * Implementation is using a HashTable. Each wrap has a set which maintains the elements in that wrap.
  * When 2 wraps are union, then both the set's are clubbed. O(log(n)) operation
  * A HashMap is also maintained to find the Wrap associated with each node. O(log(n)) operation in mainitaining it.
  *
  * If the input array is null at any index, it is ignored
  */

class UnionFind[T](all: Iterable[T]) {
  private var dataStruc = new HashMap[T, Wrap]
  for (a <- all )
    dataStruc = dataStruc + (a -> new Wrap(a))


  /**
    * THe number of Unions
    */
  private var size0 = dataStruc.size
  def size = size0

  /**
    * Unions the set containing a and b
    */
  def union(a: T, b: T): Wrap = {

    val first: Wrap = dataStruc.get(a).get
    val second: Wrap = dataStruc.get(b).get
    if (first.contains(b) || second.contains(a))
      first
    else {
      //below is to merge smaller with bigger rather than other way around
      val firstIsBig = first.set.size > second.set.size
      val ans = if (firstIsBig) {
        first.set = first.set ++ second.set
        second.set.foreach(a => {
          dataStruc = dataStruc - a
          dataStruc = dataStruc + (a -> first)
        })
        first
      } else {
        second.set = second.set ++ first.set
        first.set.foreach(a => {
          dataStruc = dataStruc - a
          dataStruc = dataStruc + (a -> second)
        })
        second
      }
      size0 = size0 - 1
      ans

    }

  }

  /**
    * true if they are in same set. false if not
    */
  def find(a: T, b: T): Boolean =
    dataStruc.get(a).get.contains(b)

  class Wrap(e: T) {
    var set = new HashSet[T]
    set = set + e

    def add(elem: T) {
      set = set + elem
    }

    def contains(elem: T): Boolean = set.contains(elem)

  }

}
