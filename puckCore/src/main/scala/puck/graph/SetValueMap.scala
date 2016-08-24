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

package puck.graph


abstract class CollectionHandler[C[_]] {
  def isEmpty(c : C[_]) : Boolean
  def empty[V] : C[V]
  def iterator[V](c : C[V]) : Iterator[V]
  def add[V](c : C[V], elt : V) : C[V]
  def remove[V](c : C[V], elt : V) : C[V]
  def contains[V](c : C[V], elt : V) : Boolean
  def toList[V](c : C[V]) : List[V]
  def map[V, W](c: C[V], f : V => W) : C[W]
}

object CollectionValueMap {
  val listHandler = new CollectionHandler[List] {

    def isEmpty(c : List[_]) : Boolean = c.isEmpty

    def empty[V]: List[V] = List.empty

    def iterator[V](c : List[V]) : Iterator[V] = c.iterator

    def toList[V](c: List[V]): List[V] = c

    def remove[V](c: List[V], elt: V): List[V] = c filter (_ != elt)

    def contains[V](c: List[V], elt: V): Boolean = c contains elt

    def add[V](c: List[V], elt: V): List[V] = elt :: c

    def map[V, W](c: List[V], f: (V) => W): List[W] = c map f
  }
  val setHandler = new CollectionHandler[Set] {

    def isEmpty(c : Set[_]) : Boolean = c.isEmpty

    def empty[V]: Set[V] = Set.empty

    def iterator[V](c : Set[V]) : Iterator[V] = c.iterator

    def toList[V](c: Set[V]): List[V] = c.toList

    def remove[V](c: Set[V], elt: V): Set[V] = c - elt

    def contains[V](c: Set[V], elt: V): Boolean = c contains elt

    def add[V](c: Set[V], elt: V): Set[V] = c + elt

    def map[V, W](c: Set[V], f: (V) => W): Set[W] = c map f
  }

}

class CollectionValueMap[K, C[_], V]
(val content : Map[K, C[V]],
 val handler : CollectionHandler[C]){

  override def toString = "CollectionValueMap" + content.mkString("(", ",\n", ")")

  def get(key : K) = content get key

  def getFlat(key : K) : C[V] = content.getOrElse(key, handler.empty)

  def + (key :K , v : V) : CollectionValueMap[K, C, V] = {
    val values : C[V] = content getOrElse (key, handler.empty)
    val vs : C[V] = handler.add[V](values, v)
    new CollectionValueMap[K, C, V](content + (key -> vs), handler)
  }

  def - (key : K) : CollectionValueMap[K, C, V] =
    new CollectionValueMap[K, C, V](content - key, handler)

  def - (key : K,  v: V) : CollectionValueMap[K, C, V] = {
    val values : C[V]= content getOrElse (key, handler.empty)
    val newValues = handler.remove(values, v)

    if(handler.isEmpty(newValues))
      this - key
    else
      new CollectionValueMap[K, C, V](content + (key -> newValues), handler)

  }

  def mapValues[W](f : V => W) =
    new CollectionValueMap[K, C, W]( content mapValues ( s => handler.map(s,f) ), handler)

  def bind( key : K, v: V) : Boolean =
    (content get key) exists { handler.contains(_, v)}

  def toSeq :Seq[(K, C[V])]= content.toSeq
  def toList : List[(K, C[V])] = content.toList

  def iterator : Iterator[(K,V)] =
    for {
      s <- content.iterator
      (k, vs) = s
      v <- handler.iterator(vs)
    } yield (k, v)

  def flatList : List[(K,V)] = iterator.toList

  
  

}




object SetValueMap {
  type T[K,V] = CollectionValueMap[K,Set,V]
  def apply[K,V]() = new CollectionValueMap[K,Set,V](Map(), CollectionValueMap.setHandler)
}
object ListValueMap {
  type T[K,V] = CollectionValueMap[K,List,V]
  def apply[K,V]() = new CollectionValueMap[K,List,V](Map(), CollectionValueMap.listHandler)
}
