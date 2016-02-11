package puck.util

//thanks http://codeslashslashcomment.com/2012/03/11/heterogeneous-maps-and-keys-with-phantom-types-in-scala/
//and https://github.com/kennknowles/scala-heterogeneous-map

trait HMap[TypedKey[_]] { self =>
  def get[T](key: TypedKey[T]) : Option[T]
  def getOrElse[T](key: TypedKey[T], default : T) : T = get(key) getOrElse default
  def put[T](key: TypedKey[T], value: T) : HMap[TypedKey]
}

object HMap {
  private class WrappedMap[TypedKey[_]](m: Map[TypedKey[_], AnyRef]) extends HMap[TypedKey] {
    def get[T](key: TypedKey[T]) : Option[T] = m.get(key).asInstanceOf[Option[T]]
    def put[T](key: TypedKey[T], value: T) : HMap[TypedKey] =
      new WrappedMap[TypedKey](m + (key -> value.asInstanceOf[AnyRef]))
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
  def StringKey[T : Manifest](v : String) : StringKey[T] = new HMapKey[String, T](v)
  implicit def stringKey2String(k : StringKey[_]) : String = k.v
}
