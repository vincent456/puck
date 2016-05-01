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

//thanks http://codeslashslashcomment.com/2012/03/11/heterogeneous-maps-and-keys-with-phantom-types-in-scala/
//and https://github.com/kennknowles/scala-heterogeneous-map

trait HMap[TypedKey[_]] { self =>
  def get[T](key: TypedKey[T]) : Option[T]
  def getOrElse[T](key: TypedKey[T], default : T) : T = get(key) getOrElse default
  def put[T](key: TypedKey[T], value: T) : HMap[TypedKey]
  def remove[T](key: TypedKey[T]) : HMap[TypedKey]
  def keys : Iterable[TypedKey[_]]
}

object HMap {
  private class WrappedMap[TypedKey[_]](m: Map[TypedKey[_], AnyRef]) extends HMap[TypedKey] {
    def get[T](key: TypedKey[T]) : Option[T] = m.get(key).asInstanceOf[Option[T]]
    def put[T](key: TypedKey[T], value: T) : HMap[TypedKey] =
      new WrappedMap[TypedKey](m + (key -> value.asInstanceOf[AnyRef]))

    def remove[T](key: TypedKey[T]) : HMap[TypedKey] = new WrappedMap[TypedKey](m - key)
    def keys : Iterable[TypedKey[_]] = m.keys
  }

  def empty[TypedKey[_]] : HMap[TypedKey] = new WrappedMap[TypedKey](Map())

  case class HMapKey[T, Phantom: Manifest](v: T) {
    private val m = implicitly[Manifest[Phantom]]

    override def equals(other: Any) = other.isInstanceOf[HMapKey[T, Phantom]] && {
      val otherPh = other.asInstanceOf[HMapKey[T, Phantom]]
      (otherPh.m.runtimeClass == this.m.runtimeClass) && (otherPh.v == this.v)
    }

    override def hashCode = (v, implicitly[Manifest[Phantom]].hashCode).hashCode
    override def toString = "WithPhantom[%s](%s)".format(this.m.runtimeClass.getName, v)
  }

  type IntKey[T] = HMapKey[Int, T]
  type StringKey[T] = HMapKey[String, T]
  def  StringKey[T : Manifest](v : String) : StringKey[T] = new HMapKey[String, T](v)
  implicit def stringKey2String(k : StringKey[_]) : String = k.v
}
